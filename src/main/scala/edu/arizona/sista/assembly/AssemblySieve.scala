package edu.arizona.sista.assembly

import com.typesafe.scalalogging.StrictLogging
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.RuleReader
import edu.arizona.sista.reach.mentions.BioRelationMention
import edu.arizona.sista.reach.mentions._
import scala.collection.mutable


trait AssemblySieve extends StrictLogging {

  // the label to be used by RelationMentions constructed during assembly
  val label = "Assembly"
  val labels = Seq(label)
  // the name of the sieve
  def name = this.getClass.getSimpleName

  // Takes mentions and produces an AssemblyGraph
  def assemble(mentions:Seq[Mention]): AssemblyGraph

  // Applies a set of assembly rules using a state comprised of only
  // those mentions relevant for the task (see filterAssemblyCandidates)
  //
  // Care is taken to apply the rules to each set of mentions from the same Doc.
  def assemblyViaRules(rulesPath: String, reachOutput:Seq[Mention]):Seq[RelationMention] = {

    // read rules and initialize state with existing mentions
    val rules:String = RuleReader.readResource(rulesPath)
    val ee = ExtractorEngine(rules)
    // a subset of the mentions found by REACH, filtered for assembly
    val validMentions = Constraints.filterAssemblyCandidates(reachOutput)
    // since we break a paper into sections, we'll need to group the mentions by doc
    // rule set only produces target RelationMentions

    val assembledMentions:Iterable[RelationMention] =
      for {
        (doc, mentionsFromReach) <- validMentions.groupBy(_.document)
        // create a new state with just the mentions from a particular doc
        // note that a doc is as granular as a section of a paper
        oldState = State(mentionsFromReach)
        // get parents of each
        parents = Constraints.mkParentsMap(mentionsFromReach)
        // extract the assembly mentions from the subset of reach mentions
        // belonging to the same doc.
        // NOTE: Odin expects all mentions in the state to belong to the same doc!
        m <- ee.extractFrom(doc, oldState)
      } yield m.asInstanceOf[RelationMention]

    assembledMentions.toSeq
  }

  def assembleAndFilter(mentions:Seq[Mention]):AssemblyGraph = {
    val links =
      // Before and After must be PossibleControllers
      // If either is an entity, it must have a PTM modification
      Constraints.imposeAssemblyConstraints(
        assemble(mentions).connected
      )

    // create the map of m -> parents for the filtering of results
    val parents = Constraints.mkParentsMap(mentions)
    // ensure we're not "re-assembling" existing regulations
    // I don't think we should bother filtering these out,
    // but some might say that not doing so inflates the contribution of the sieve...
    val validLinks = links.filter(link =>
      Constraints.differentParentMentions(parents, link) && Constraints.notAnExistingComplexEvent(link)
    )

    AssemblyGraph(validLinks, this.name)
  }

  // Filtering for IO-based sieves
  def assembleAndIOFilter(mentions:Seq[Mention]):AssemblyGraph = {
    val links =
    // For IO sieves, "before" should be an Event
      Constraints.beforeMustBeEvent(
        // general constraints on assembly "links"
        Constraints.imposeAssemblyConstraints(
          // apply sieve's assembly
          assemble(mentions).connected
        )
      )

    // create the map of m -> parents for the filtering of results
    val parents = Constraints.mkParentsMap(mentions)
    // ensure we're not "re-assembling" existing regulations
    // I don't think we should bother filtering these out,
    // but some might say that not doing so inflates the contribution of the sieve...
    val validLinks = links.filter(link =>
      Constraints.differentParentMentions(parents, link) && Constraints.notAnExistingComplexEvent(link)
    )

    AssemblyGraph(validLinks, this.name)
  }
}

/**
 * Domain-specific sieve.
 * Check to see if the output of a mention is the input to another
 */
class ExactIOSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    logger.debug(s"Beginning ${this.name} assembly...")

    // find pairs that satisfy strict IO conditions
    // input of m1 must be output of m2 OR input of m2 must be output m1
    val links:Seq[BioRelationMention] =
      for {
        m1 <- mentions
        // get output representation for m1
        m1inputs = IOResolver.getInputs(m1)
        // get output representation for m1
        m1outputs = IOResolver.getOutputs(m1)
        // compare m1's IO to the IO of every other mention
        m2 <- mentions
        // get output representation for m1
        m2inputs = IOResolver.getInputs(m2)
        // get output representation for m2
        m2outputs = IOResolver.getOutputs(m2)
        // don't link if these mentions are the same
        if m1 != m2
        // only yield if the strict IO constraint holds
        // all outputs of one of the mentions must be contained by the inputs of the other mention
        if m1outputs.isSubsetOf(m2inputs) || m2outputs.isSubsetOf(m1inputs)
      } yield {
        val result = (m1, m2) match {
          // all of mention 1's output serves is contained by the input to mention 2
          case (incoming, outgoing) if m1outputs.isSubsetOf(m2inputs) =>
            // the assembly link
            new BioRelationMention(labels = this.labels,
              arguments = Map(Architecture.predecessor -> Seq(incoming), Architecture.successor -> Seq(outgoing)),
              sentence = incoming.sentence,
              document = incoming.document,
              keep = true,
              foundBy = this.name)
          // all of mention 2's output serves is contained by the input to mention 1
          case (outgoing, incoming) if m2outputs.isSubsetOf(m1inputs) =>
            // the assembly link
            new BioRelationMention(labels = this.labels,
              arguments = Map(Architecture.predecessor -> Seq(incoming), Architecture.successor -> Seq(outgoing)),
              sentence = incoming.sentence,
              document = incoming.document,
              keep = true,
              foundBy = this.name)
        }
        result
      }
    AssemblyGraph(links, this.name)
  }

  override def assembleAndFilter(mentions:Seq[Mention]):AssemblyGraph = assembleAndIOFilter(mentions)
}

/**
 * Domain-specific sieve.
 * Check to see if the output of a mention is the input to another
 */
class ApproximateIOSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    logger.debug(s"Beginning ${this.name} assembly...")
    // find pairs that satisfy approximate IO conditions
    // input of m1 must be approximate output of m2 OR input of m2 must be approximate output m1
    // see IO.fuzzyMatch for details on meaning of "approximate"
    val links:Seq[BioRelationMention] =
      for {
        m1 <- mentions
        // get output representation for m1
        m1inputs = IOResolver.getInputs(m1)
        // get output representation for m1
        m1outputs = IOResolver.getOutputs(m1)
        // compare m1's IO to the IO of every other mention
        m2 <- mentions
        // get output representation for m1
        m2inputs = IOResolver.getInputs(m2)
        // get output representation for m2
        m2outputs = IOResolver.getOutputs(m2)
        // don't link if these mentions are the same
        if m1 != m2
        // only yield if the approximate IO constraint holds
        if m1outputs.isFuzzySubsetOf(m2inputs) || m2outputs.isFuzzySubsetOf(m1inputs)
      } yield {
        val result = (m1, m2) match {
          // mention 2's input is coming from the output of mention 1
          case (incoming, outgoing) if m1outputs.isFuzzySubsetOf(m2inputs) =>
            // the assembly link
            new BioRelationMention(labels = this.labels,
              arguments = Map(Architecture.predecessor -> Seq(incoming), Architecture.successor -> Seq(outgoing)),
              sentence = incoming.sentence,
              document = incoming.document,
              keep = true,
              foundBy = this.name)
          // mention 1's input is coming from the output of mention 2
          case (outgoing, incoming) if m2outputs.isFuzzySubsetOf(m1inputs) =>
            // the assembly link
            new BioRelationMention(labels = this.labels,
              arguments = Map(Architecture.predecessor -> Seq(incoming), Architecture.successor -> Seq(outgoing)),
              sentence = incoming.sentence,
              document = incoming.document,
              keep = true,
              foundBy = this.name)
        }
        result
      }
    AssemblyGraph(links, this.name)
  }

  override def assembleAndFilter(mentions:Seq[Mention]):AssemblyGraph = assembleAndIOFilter(mentions)
}

/**
 * Linguistic sieve (intra-sentence).
 * Run Odin syntactic dep rules to see if mentions are linked by a prep
 */
class PrepositionLinkSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    logger.debug(s"Beginning ${this.name} assembly...")
    // read rules and initialize state with existing mentions
    val p = "/edu/arizona/sista/assembly/grammar/assembly.yml"
    val assembledMentions = assemblyViaRules(p, mentions)
    AssemblyGraph(assembledMentions, this.name)
  }
}

class InterSentenceLinguisticSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    val p = "/edu/arizona/sista/assembly/grammar/cross-sentence-assembly.yml"
    val assembledMentions:Seq[RelationMention] = assemblyViaRules(p, mentions)
    AssemblyGraph(assembledMentions, this.name)
  }
}

// A set of constraints used to validate the output of sieves
object Constraints {

  // roles for assembly links
  val beforeRole = "before"
  val afterRole = "after"

  // Determines valid candidates for rule-based assembly
  def filterAssemblyCandidates(reachOutput:Seq[Mention]):Seq[Mention] = reachOutput.filter(_ matches "PossibleController")

  // Builds a parent LUT to be used for filtering mentions
  def mkParentsMap(mentions:Seq[Mention]):Map[Mention, Set[Mention]] = {

    val parents = mutable.Map[Mention, mutable.Set[Mention]]().withDefaultValue(mutable.Set.empty)
    for {
      m <- mentions
      // for each key
      role <- m.arguments.keys
      // for each value
      arg <- m.arguments(role)
    } {
      // arg points to its parents
      parents(arg) += m
    }
    // frozen in stone for a thousand years...
    parents.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)
  }

  // "before" and "after" should not share a parent
  def differentParentMentions(parents:Map[Mention, Set[Mention]], link:Mention):Boolean = {
    val before:Mention = link.arguments(Constraints.beforeRole).head
    val after:Mention = link.arguments(Constraints.afterRole).head
    // arg parentage should not intersect
    parents(before).intersect(parents(after)).isEmpty
  }

  // For a complex event "C", with a controlled "A", do not re-create "A precedes C"
  def notAnExistingComplexEvent(link: Mention):Boolean = {
    val before = link.arguments(beforeRole).head
    val after = link.arguments(afterRole).head
    val argsOfBeforeMention:Set[Mention] = before.arguments.values.flatten.toSet
    val argsOfAfterMention:Set[Mention] = after.arguments.values.flatten.toSet

    !(argsOfBeforeMention contains after) && !(argsOfAfterMention contains before)
  }

  // test if PTM
  def hasPTM(m: Mention): Boolean = m.toBioMention.modifications.exists(_.isInstanceOf[PTM])

  // Restrict "before" to Event
  // Used by IO Sieves
  def beforeMustBeEvent(ams: Seq[RelationMention]):Seq[RelationMention] = {
    for {
      am <- ams
      // size should be 1
      before = am.arguments(beforeRole).head
      if before matches "Event"
    } yield am
  }

  // constraints on assembled mentions to avoid redundancies
  // only return assembled mentions
  // where both "before" and "after" args are
  // PossibleControllers (see reach/biogrammar/taxonomy.yml)
  // (i.e. ignore mentions related to context, cell types, species, etc)
  // an assembled mention should not join the same Entity
  // (i.e. "before" & "after" should not both point to Entity e)
  def imposeAssemblyConstraints(ams: Seq[RelationMention]):Seq[RelationMention] = {
    for {
      am <- ams
      // both should be of size 1
      before = am.arguments(beforeRole).head
      after = am.arguments(afterRole).head
      outputsOfBefore = IOResolver.getOutputs(before)
      inputsOfAfter = IOResolver.getInputs(after)
      outputsOfAfter = IOResolver.getOutputs(after)
      // only assemble things that involve PossibleControllers
      // lhs and rhs ("after") should be Events
      // entities don't transform input
      if before matches "PossibleController"
      // NOTE: some unresolved coref mentions linger in the reach output
      // ex (PMC3847091):  ... that this phosphorylation leads to its increased translocation to the cytosol and nucleus and increased binding to p53 (PMC3847091)
      if ! ( before matches "Generic_event")  && ! (after matches "Generic_event")
      // before and after can only be entities iff they have a PTM modification
      if (before matches "Event") || Constraints.hasPTM(before)
      if (after matches "Event") || Constraints.hasPTM(after)
      // ensure output of "before" != output of "after"
      if outputsOfBefore != outputsOfAfter
      // input of "after" != output of "after"
      // i.e. there should be a change of state in the IO
      if inputsOfAfter != outputsOfAfter
    } yield am
  }
}

// TODO: Add sieve to convert regs into Assembly mentions (A causes B, C leads to D, etc)

object SieveManager {
//  // The default sieve
//  class IdentitySieve extends AssemblySieve {
//    // do nothing
//    def assemble(mentions:Seq[Mention]) = AssemblyGraph(Nil, "")
//  }
  // alternative to reflection for lookup
  val lut =
    Map[String, AssemblySieve](
      "ExactIOSieve" -> new ExactIOSieve,
      "ApproximateIOSieve" -> new ApproximateIOSieve,
      "PrepositionLinkSieve" -> new PrepositionLinkSieve)
      // if the key is missing from the map, return an IdentitySieve
      //.withDefaultValue(new IdentitySieve)
}
