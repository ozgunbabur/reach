package edu.arizona.sista.assembly

import com.typesafe.scalalogging.StrictLogging
import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.mentions._

import scala.collection.SetLike



case class Entity(id: String, features: Set[String], text: String) {

  // Customized equality
  override def equals(o: Any) = o match {
    // Check that the ID and mods are the same
    // Could be input or output
    case that: Entity => that.id.equalsIgnoreCase(this.id) && that.features.equals(this.features)
    case _ => false
  }
  // Fuzzy match over IO, checking Grounding-based id OR text
  def fuzzyMatch(that:Entity): Boolean = this.id.equalsIgnoreCase(that.id) || this.text.equalsIgnoreCase(that.text)
  // Check to see if this IO is the Input of some Mention
  // Uses custom equality def (text doesn't need to match)
  def isInputOf(m: Mention): Boolean = IOResolver.getInputs(m).forall(i => this.equals(i))
  def isContainedByInputOf(m: Mention): Boolean = IOResolver.getInputs(m).exists(_.equals(this))
  // Check to see if this IO is the Output of some Mention
  // Uses custom equality def (text doesn't need to match)
  def isOutputOf(m: Mention): Boolean = IOResolver.getOutputs(m).forall(o => this.equals(o))
  def isContainedByOutputOf(m: Mention): Boolean = IOResolver.getOutputs(m).exists(_.equals(this))
  // Check to see if this IO is the Output of some Mention
  // ignores differences in the features
  def isFuzzyOutputOf(m: Mention): Boolean = IOResolver.getOutputs(m).exists(o => this.fuzzyMatch(o))
  // Check to see if this IO is the Output of some Mention
  // ignores differences in the features
  def isFuzzyInputOf(m: Mention): Boolean = IOResolver.getInputs(m).exists(i => this.fuzzyMatch(i))
}

case class Event(participants:Map[String, IOSet], input:IOSet, output:IOSet, label:String, var evidence:Seq[Mention]) {

}

// Because an Event's input can involve more than Entity,
// we represent the input to an event as a set of inputs
// This class provides methods to simplify comparisons between these sets
//
// NOTE: though I am unaware of any cases of an event producing multiple outputs,
// a decision was made to represent outputs as a set of outputs as well.
// I have switched back to using a single type (IO) to represent both inputs and outputs.
// So far this has not been a problem.
class IOSet(m: Set[Entity]) extends Set[Entity] with SetLike[Entity, IOSet] with Serializable {

  private val members = m.toSet
  override def empty:IOSet = new IOSet(Set.empty[Entity])
  def + (elem: Entity):IOSet = new IOSet(members ++ Set(elem))
  def ++ (elem: IOSet):IOSet = new IOSet(members ++ elem)
  def - (elem: Entity):IOSet = new IOSet(members.filterNot(_.equals(elem)))
  def -- (elem: IOSet):IOSet = new IOSet(members -- elem)
  def contains(elem: Entity):Boolean = members.exists(_.equals(elem))
  def fuzzyContains(elem: Entity) = members.exists(_.fuzzyMatch(elem))

  def iterator:Iterator[Entity] = members.iterator

  def intersection(that:IOSet):IOSet = {
    IOSet(members.filter(m => that contains m))
  }
  def fuzzyIntersection(that:IOSet):IOSet = {
    IOSet(members.filter(m => that fuzzyContains m))
  }

  def intersects(that:IOSet):Boolean = members.exists(io => that contains io)
  def fuzzyIntersects(that:IOSet):Boolean = members.exists(io => that fuzzyContains io)

  // Customized equality
  override def equals(o: Any) = o match {
    // for convenience, implicitly convert a naked IO to an IOSet for comparison
    case that: Entity => this == IOSet(that)
    // sets must be the same size
    case anotherSet: IOSet => (anotherSet.size == this.size) && this.isSubsetOf(anotherSet)
    case _ => false
  }

  // All members of A in B
  def isSubsetOf(that: IOSet):Boolean = members.forall(m => that.exists(_.equals(m)))
  def isSubsetOf(that: Entity):Boolean = this isSubsetOf IOSet(that)

  // All members of A in B, but B is larger
  def isProperSubsetOf(that: IOSet):Boolean = this.size < that.size && this.isSubsetOf(that)
  def isProperSubsetOf(that: Entity):Boolean = this isProperSubsetOf IOSet(that)

  // All members of A have fuzzyMatch membership in B
  def isFuzzySubsetOf(that: IOSet):Boolean = members.forall(m => that.exists(_.fuzzyMatch(m)))
  def isFuzzySubsetOf(that: Entity):Boolean = this isFuzzySubsetOf IOSet(that)

  // All members of A have fuzzyMatch membership in B, but B is larger
  def isProperFuzzySubsetOf(that: IOSet):Boolean = this.size < that.size && this.isFuzzySubsetOf(that)
  def isProperFuzzySubsetOf(that: Entity):Boolean = this isProperFuzzySubsetOf IOSet(that)

  // NOTE: complete means that A & B are the same size AND all IO instances of A are in input of B
  def isCompleteInputOf(m: Mention):Boolean = this == IOResolver.getInputs(m)
  def isCompleteFuzzyInputOf(m: Mention):Boolean = {
    val that = IOResolver.getInputs(m)
    this.size == that.size && this.isFuzzySubsetOf(that)
  }

  def isCompleteOutputOf(m: Mention):Boolean = this == IOResolver.getOutputs(m)
  def isCompleteFuzzyOutputOf(m: Mention):Boolean = {
    val that = IOResolver.getOutputs(m)
    this.size == that.size && this.isFuzzySubsetOf(that)
  }

  def isPartialInputOf(m: Mention):Boolean = {
    val that = IOResolver.getInputs(m)
    // NOTE: this is NOT the same as isProperSubsetOf; only one member of A has to exist in B
    this.intersects(that) && this.size < that.size
  }
  def isPartialFuzzyInputOf(m: Mention):Boolean = {
    val that = IOResolver.getInputs(m)
    // NOTE: this is NOT the same as isProperSubsetOf; only one member of A has to fuzzyMatch a member of B
    this.fuzzyIntersects(that) && this.size < that.size
  }
}

object IOSet {
  def empty = new IOSet(Set.empty[Entity])
  def apply(io: Entity):IOSet = new IOSet(Set(io))
  def apply(io: Iterable[Entity]):IOSet = new IOSet(io.toSet)
  def apply(): IOSet = empty
}

object IOResolver extends StrictLogging {
  val THEME = "theme"
  val CAUSE = "cause"
  val CONTROLLED = "controlled"
  val CONTROLLER = "controller"

  def findPatients(m: Mention):Seq[Mention] = m match {
    // avoid recursive calls, as too much information can be lost
    case hasTheme if hasTheme.arguments contains THEME => hasTheme.arguments(THEME)
    case naked if naked.arguments.isEmpty => Seq(naked)
    case hasControlled if hasControlled.arguments contains CONTROLLED => hasControlled.arguments(CONTROLLED)
  }

  def findAgents(m: Mention):Seq[Mention] = m match {
    // avoid recursive calls, as too much information can be lost
    case hasCause if hasCause.arguments contains CAUSE => hasCause.arguments(CAUSE)
    case hasController if hasController.arguments contains CONTROLLER => hasController.arguments(CONTROLLER)
    case naked if naked.arguments.isEmpty => Seq(naked)
  }

  def getGroundingIDasString(m: Mention): String = {
    //logger.debug(s"\t\tattempting to ground ${m.label} (${m.text})")
    val unknown = s"UNKNOWN-${m.text.toLowerCase}"
    m.toBioMention match {
      // Some remnants of coref linger...
      case corefOrphan if corefOrphan matches "Generic_event" => unknown
      // An entity should be easy to ground...
      case tb : BioTextBoundMention => if (tb.isGrounded) tb.xref.get.printString else unknown
      // Recursively unpack this guy
      // TODO: figure out a better way to do this than .head
      case hasPatient if findPatients(m).nonEmpty => getGroundingIDasString(findPatients(m).head)
      // Cannot be determined...
      case _ => unknown
    }
  }

  // Get labels for PTM and Mutant mods
  def getRelevantFeatures(m: Mention): Set[String] = {
    val mods = m.toBioMention.modifications.flatMap {
      case ptm: PTM => Set(ptm.label)
      case mutant: Mutant => Set(mutant.label)
      case _ => Set.empty[String]
    }
    // add mention's label!
    mods ++ Set(m.label)
  }

  // Represent the input to a Mention
  // TODO: handle remaining special cases (Hydrolysis, Translocation)
  def getInputs(mention: Mention):IOSet = mention match {

    // Use the grounding ID of the TextBoundMention
    // TODO: should this only apply if btm.matches("Entity")?
    case btm: BioTextBoundMention =>
      //logger.debug(s"getting inputs for TB...")
      val id = getGroundingIDasString(btm)
      val text = btm.text
      val features = getRelevantFeatures(btm)
      //logger.debug(s"successfully constructed inputs for TB!")
      IOSet(Entity(id, features, text))

    // get output of all themes
    case binding: BioMention if binding.matches("Binding") && binding.arguments.contains(THEME) =>
      //logger.debug(s"getting inputs for Binding...")
      val input:Seq[Entity] =
        for {
          theme:Mention <- binding.arguments(THEME)
          id = getGroundingIDasString(theme)
          text = theme.text
          features:Set[String] = getRelevantFeatures(theme)
        } yield Entity(id, features, text)
      //logger.debug(s"successfully constructed inputs for Binding!")
      IOSet(input)

    // Is it an BioEventMention with a theme?
    case bemWithTheme: BioMention if bemWithTheme.matches("SimpleEvent") && bemWithTheme.arguments.contains(THEME) =>
      // get output of each theme
      //logger.debug(s"getting inputs for SimpleEvent...")
      val input:Seq[Entity] =
        for {
          theme:Mention <- bemWithTheme.arguments(THEME)
          id = getGroundingIDasString(theme)
          text = theme.text
          // Get relevant modifications of theme (including its label)
          features = getRelevantFeatures(theme)
        } yield Entity(id, features, text)
      //logger.debug(s"successfully constructed inputs for SimpleEvent!")
      IOSet(input)

    // Do we have a proper (valid) regulation?
    case reg: BioMention if reg.matches("ComplexEvent") && reg.arguments.contains(CONTROLLER) && reg.arguments.contains(CONTROLLED) =>
      // attempt to find the Agent and its Inputs
      //logger.debug(s"getting inputs for reg...")
      val inputsOfAgents:Seq[Entity] = findAgents(reg).flatMap(getInputs)
      val inputsOfPatients:Seq[Entity] = findPatients(reg).flatMap(getInputs)
      //logger.debug(s"successfully constructed inputs for reg!")
      IOSet(inputsOfAgents ++ inputsOfPatients)

    // TODO: figure out what should be done here
    case hydrolysis: BioMention if hydrolysis matches "Hydrolysis" => IOSet.empty

    // TODO: figure out what should be done here
    case translocation: BioMention if translocation matches "Translocation" => IOSet.empty

    // TODO: What is being left out?
    case _ => IOSet.empty
  }

  // Represent the output of a Mention
  // TODO: handle remaining special cases (Hydrolysis, Translocation)
  def getOutputs(mention: Mention): IOSet = mention match {

    // Use the grounding ID of the TextBoundMention
    // TODO: should this only apply if btm.matches("Entity")?
    case btm: BioTextBoundMention =>
      //logger.debug(s"getting outputs for TB...")
      val outputs = for (i <- getInputs(mention)) yield Entity(i.id, i.mods, i.text)
      //logger.debug(s"successfully constructed inputs for TB!")
      IOSet(outputs)

    // TODO: Should this be 1 output (i.e. the complex?)
    // If so, how should the grounding ID work?
    // get output of all themes
    case binding: BioMention if binding.matches("Binding") && binding.arguments.contains(THEME) =>
      //logger.debug(s"getting outputs for Binding...")
      val output:Seq[Entity] =
        for {
          // process each binding participant
          theme:Mention <- binding.arguments(THEME)
          id = getGroundingIDasString(theme)
          text = theme.text
          // include binding label
          features:Set[String] = getRelevantFeatures(theme) ++ Set(binding.label)
        } yield Entity(id, features, text)
      //logger.debug(s"successfully constructed inputs for Binding!")
      IOSet(output)

    // Is it an BioEventMention with a theme?
    case bemWithTheme: BioMention if bemWithTheme.matches("SimpleEvent") && bemWithTheme.arguments.contains(THEME) =>
      //logger.debug(s"getting outputs for SimpleEvent...")
      // get input of each theme
      val input = getInputs(bemWithTheme)
      // add the event's label (PTM) to the set of features for each input
      val output = input.map(i => Entity(i.id, i.features ++ Set(bemWithTheme.label), i.text))
      //logger.debug(s"successfully constructed inputs for SimpleEvent!")
      IOSet(output)

    // Do we have a proper (valid) regulation?
    case reg: BioMention if reg.matches("ComplexEvent") && reg.arguments.contains(CONTROLLER) && reg.arguments.contains(CONTROLLED) =>
      //logger.debug(s"getting outputs for ComplexEvent...")
      val output =
        for {
          // process each controlled
          controlled <- reg.arguments(CONTROLLED)
          //_ = logger.debug("\tSearching for groundingID")
          id = getGroundingIDasString(controlled)
          //_ = logger.debug("\tFound groundingID")
          text = controlled.text
          // attempt to convert each controlled into its base entity
          patients = findPatients(controlled)
          //_ = logger.debug("\tFound patients")
          // get the mods of the base entity for each assumed patient
          featuresFromPatients = patients.flatMap(getRelevantFeatures).toSet
        } yield {
          // assemble the mods per reg output
          val features =
            // the mods of the theme (including its label)
            getRelevantFeatures(controlled) ++
            // the mods of the theme of the theme
            featuresFromPatients
          //val _ = logger.debug("\tFound relevantMods")
          // a single output per controlled
          Entity(id, features, text)
      }
      //logger.debug(s"successfully constructed inputs for ComplexEvent!")
      IOSet(output)

    // TODO: figure out what should be done here
    case hydrolysis: BioMention if hydrolysis matches "Hydrolysis" => IOSet.empty

    // TODO: figure out what should be done here
    case translocation: BioMention if translocation matches "Translocation" => IOSet.empty

    // TODO: What is being left out?
    case _ => IOSet.empty
  }

  // Check to see if a Mention has input
  def hasInput(m: Mention): Boolean = getInputs(m).nonEmpty

  // Check to see if a Mention has output
  def hasOutput(m: Mention): Boolean = getOutputs(m).nonEmpty
}
