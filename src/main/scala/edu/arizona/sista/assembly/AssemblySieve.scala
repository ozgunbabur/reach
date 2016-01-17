package edu.arizona.sista.assembly

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.RuleReader
import edu.arizona.sista.reach.mentions.BioRelationMention


trait AssemblySieve {

  // the label to be used by RelationMentions constructed during assembly
  val label = "Assembly"
  val labels = Seq(label)
  // the name of the sieve
  def name = this.getClass.getName

  // takes mentions and produces an AssemblyGraph
  def assemble(mentions:Seq[Mention]): AssemblyGraph

  // constraints on assembled mentions to avoid redundancies
  // only return assembled mentions
  // where both "before" and "after" args are
  // PossibleControllers (see reach/biogrammar/taxonomy.yml)
  // (i.e. ignore mentions related to context, cell types, species, etc)
  // an assembled mention should not join the same Entity
  // (i.e. "before" & "after" should not both point to Entity e)
  def filterAssembled(am: Seq[RelationMention]):Seq[RelationMention]= {
    am.filter{ a =>
      val before = a.arguments("before").head
      val after = a.arguments("after").head
      // only assemble things that involve PossibleControllers
      (before matches "PossibleController") && (after matches "PossibleController")
    }.filterNot{ a =>
      val before = a.arguments("before").head
      val after = a.arguments("after").head
      // remove assembled mentions where the before and after is the same Entity
      (IOResolver.getOutputs(before) == IOResolver.getOutputs(after)) &&
        (a.arguments("before").head matches "Entity") &&
        (a.arguments("after").head matches "Entity")
    }
    //TODO: enforce constraint that ensures output of "before" != output of "after"
  }

  def assembleAndFilter(mentions:Seq[Mention]):AssemblyGraph = {
    AssemblyGraph(filterAssembled(assemble(mentions).connected), this.name)
  }
}

/**
 * Domain-specific sieve.
 * Check to see if the output of a mention is the input to another
 */
class ExactIOSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    // find pairs that satisfy strict IO conditions
    // input of m1 must be output of m2 OR input of m2 must be output m1
    val links:Seq[BioRelationMention] =
      for {
        m1 <- mentions
        // get output representation for m1
        m1Outputs = IOResolver.getOutputs(m1)
        // compare m1's IO to the IO of every other mention
        m2 <- mentions
        // get output representation for m2
        m2Outputs = IOResolver.getOutputs(m2)
        // don't link if these mentions are the same
        if m1 != m2
        // only yield if the strict IO constraint holds
        if m1Outputs.isInputOf(m2) || m2Outputs.isInputOf(m1)
      } yield {
        val result = (m1, m2) match {
          // mention 2's input is coming from the output of mention 1
          case (incoming, outgoing) if m1Outputs.isInputOf(m2) =>
            // the assembly link
            new BioRelationMention(labels = this.labels,
              arguments = Map(Architecture.predecessor -> Seq(incoming), Architecture.successor -> Seq(outgoing)),
              sentence = incoming.sentence,
              document = incoming.document,
              keep = true,
              foundBy = this.name)
          // mention 1's input is coming from the output of mention 2
          case (outgoing, incoming) if m2Outputs.isInputOf(m1) =>
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
    // validate assembled mentions
    //val filteredLinks = filterAssembled(links)
    AssemblyGraph(links, this.name)
  }
}

/**
 * Domain-specific sieve.
 * Check to see if the output of a mention is the input to another
 */
class ApproximateIOSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    // find pairs that satisfy approximate IO conditions
    // input of m1 must be approximate output of m2 OR input of m2 must be approximate output m1
    // see IO.fuzzyMatch for details on meaning of "approximate"
    val links:Seq[BioRelationMention] =
      for {
        m1 <- mentions
        // get output representation for m1
        m1Outputs = IOResolver.getOutputs(m1)
        // compare m1's IO to the IO of every other mention
        m2 <- mentions
        // get output representation for m2
        m2Outputs = IOResolver.getOutputs(m2)
        // don't link if these mentions are the same
        if m1 != m2
        // only yield if the approximate IO constraint holds
        if m1Outputs.isFuzzyInputOf(m2) || m2Outputs.isFuzzyInputOf(m1)
      } yield {
        val result = (m1, m2) match {
          // mention 2's input is coming from the output of mention 1
          case (incoming, outgoing) if m1Outputs.isFuzzyInputOf(m2) =>
            // the assembly link
            new BioRelationMention(labels = this.labels,
              arguments = Map(Architecture.predecessor -> Seq(incoming), Architecture.successor -> Seq(outgoing)),
              sentence = incoming.sentence,
              document = incoming.document,
              keep = true,
              foundBy = this.name)
          // mention 1's input is coming from the output of mention 2
          case (outgoing, incoming) if m2Outputs.isFuzzyInputOf(m1) =>
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
    // validate assembled mentions
    //val filteredLinks = filterAssembled(links)
    AssemblyGraph(links, this.name)
  }
}

/**
 * Linguistic sieve (intra-sentence).
 * Run Odin syntactic dep rules to see if mentions are linked by a prep
 */
class PrepositionLinkSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {

    // read rules and initialize state with existing mentions
    val rulesPath = "/edu/arizona/sista/assembly/grammar/assembly.yml"
    val rules:String = RuleReader.readResource(rulesPath)
    val ee = ExtractorEngine(rules)
    // a subset of the mentions found by REACH, filtered for assembly
    val validMentions = mentions.filter(_ matches "PossibleController")
    // since we break a paper into sections, we'll need to group the mentions by doc
    // rule set only produces target RelationMentions

    val assembledMentions =
      for {
        (doc, mentionsFromReach) <- validMentions.groupBy(_.document)
        // create a new state with just the mentions from a particular doc
        // note that a doc is as granular as a section of a paper
        oldState = State(mentionsFromReach)
        // extract the assembly mentions from the subset of reach mentions
        // belonging to the same doc.
        // NOTE: Odin expects all mentions in the state to belong to the same doc!
        m <- ee.extractFrom(doc, oldState)
        // TODO: this may not be necessary
        // ensure that mention is one related to Assembly
        if m matches this.label
      } yield m.asInstanceOf[RelationMention]

    // validate assembled mentions
    //val filteredAssembledMentions = filterAssembled(assembledMentions)
    AssemblyGraph(assembledMentions.toVector, this.name)
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
