package edu.arizona.sista.assembly

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.mentions._

import scala.collection.SetLike


case class IO(id: String, mods: Set[String], text: String) {

  // Customized equality
  override def equals(o: Any) = o match {
    // Check that the ID and mods are the same
    // Could be input or output
    case that: IO => that.id.equalsIgnoreCase(this.id) && that.mods.equals(this.mods)
    case _ => false
  }
  // Fuzzy match over IO, checking Grounding-based id OR text
  def fuzzyMatch(that:IO): Boolean = this.id.equalsIgnoreCase(that.id) || this.text.equalsIgnoreCase(that.text)
  // Check to see if this IO is the Input of some Mention
  // Uses custom equality def (text doesn't need to match)
  def isInputOf(m: Mention): Boolean = IOResolver.getInputs(m).forall(i => this.equals(i))
  def isContainedByInputOf(m: Mention): Boolean = IOResolver.getInputs(m).exists(_.equals(this))
  // Check to see if this IO is the Output of some Mention
  // Uses custom equality def (text doesn't need to match)
  def isOutputOf(m: Mention): Boolean = IOResolver.getOutputs(m).forall(o => this.equals(o))
  def isContainedByOutputOf(m: Mention): Boolean = IOResolver.getOutputs(m).exists(_.equals(this))
  // Check to see if this IO is the Output of some Mention
  // ignores differences in the mods
  def isFuzzyOutputOf(m: Mention): Boolean = IOResolver.getOutputs(m).exists(o => this.fuzzyMatch(o))
  // Check to see if this IO is the Output of some Mention
  // ignores differences in the mods
  def isFuzzyInputOf(m: Mention): Boolean = IOResolver.getInputs(m).exists(i => this.fuzzyMatch(i))
}

class IOSet(m: Set[IO]) extends Set[IO] with SetLike[IO, IOSet] with Serializable {

  private val members = m.toSet
  override def empty: IOSet = new IOSet(Set.empty[IO])
  def + (elem: IO) : IOSet = new IOSet(members ++ Set(elem))
  def - (elem: IO) : IOSet = new IOSet(members.filterNot(_.equals(elem)))
  def contains (elem: IO) : Boolean = members.contains(elem)
  def iterator : Iterator[IO] = members.iterator

  // Customized equality
  override def equals(o: Any) = o match {
    // for convenience, implicitly convert a naked IO to an IOSet for comparison
    case that: IO => this == IOSet(that)
    case anotherSet: IOSet => (anotherSet.size == this.size) && this.forall(io => anotherSet contains io)
    case _ => false
  }

  def isSubsetOf(that: IOSet):Boolean = members.exists(m => that.exists(_.equals(m)))
  def isSubsetOf(that: IO):Boolean = this isSubsetOf IOSet(that)

  def isProperSubsetOf(that: IOSet):Boolean = members.forall(m => that.exists(_.equals(m)))
  def isProperSubsetOf(that: IO):Boolean = this isProperSubsetOf IOSet(that)

  def isFuzzySubsetOf(that: IOSet):Boolean = members.exists(m => that.exists(_.fuzzyMatch(m)))
  def isFuzzySubsetOf(that: IO):Boolean = this isFuzzySubsetOf IOSet(that)

  def isProperFuzzySubsetOf(that: IOSet):Boolean = members.forall(m => that.exists(_.fuzzyMatch(m)))
  def isProperFuzzySubsetOf(that: IO):Boolean = this isProperFuzzySubsetOf IOSet(that)

  def isOutputOf(m: Mention):Boolean = this == IOResolver.getOutputs(m)
  def isFuzzyOutputOf(m: Mention):Boolean = this isProperFuzzySubsetOf IOResolver.getOutputs(m)

  def isInputOf(m: Mention):Boolean = this == IOResolver.getInputs(m)
  def isFuzzyInputOf(m: Mention):Boolean = this isProperFuzzySubsetOf IOResolver.getInputs(m)
}

object IOSet {
  def empty = new IOSet(Set.empty[IO])
  def apply(io: IO):IOSet = new IOSet(Set(io))
  def apply(io: Iterable[IO]):IOSet = new IOSet(io.toSet)
  def apply(): IOSet = empty
}

object IOResolver {

  def findPatient(m: Mention):Option[Mention] = m match {
    case hasTheme if hasTheme.arguments contains "theme" => Some(hasTheme.arguments("theme").head)
    // Doesn't have a theme or controlled, so give up
    case failure if !(failure.arguments contains "controlled") => None
    case hasControlled if hasControlled.arguments contains "controlled" => findPatient(hasControlled.arguments("controlled").head)
  }

  def findAgent(m: Mention):Option[Mention] = m match {
    case hasCause if hasCause.arguments contains "cause" => Some(hasCause.arguments("cause").head)
    // Doesn't have a theme or controlled, so give up
    case failure if !(failure.arguments contains "controlled") => None
    case hasController if hasController.arguments contains "controller" => findPatient(hasController.arguments("controlled").head)
  }

  def getGroundingIDasString(m: Mention): String = {

    m.toBioMention match {
      case tb : BioTextBoundMention => tb.xref.get.printString
      // recursively unpack this guy
      case hasPatient if findPatient(m).nonEmpty => getGroundingIDasString(findPatient(m).get)
    }
  }

  // Get labels for PTM an Mutant mods
  def getRelevantModifications(m: Mention): Set[String] =
    m.toBioMention.modifications.flatMap {
      case ptm: PTM => Set(ptm.label)
      case mutant: Mutant => Set(mutant.label)
      case _ => Set.empty[String]
    }

  // Represent the input to a Mention
  // TODO: handle cases with multiple themes?
  def getInputs(mention: Mention):IOSet = mention match {

    // Use the grounding ID of the TextBoundMention
    // Should this only apply if btm.matches("Entity")
    case btm: BioTextBoundMention =>
      val id = btm.xref.get.printString
      val text = btm.text
      val mods = getRelevantModifications(btm)
      IOSet(IO(id, mods, text))

    // Is it an BioEventMention with a theme?
    case bemWithTheme: BioMention if bemWithTheme.matches("SimpleEvent") && bemWithTheme.arguments.contains("theme") =>
      // return the grounding id for the theme
      val m = bemWithTheme.arguments("theme").head
      val id = getGroundingIDasString(m)
      val text = m.text
      val mods =
      // Get theme's label (PTM)
        Set(m.label) ++
          // Get relevant modifications
          getRelevantModifications(m)
      IOSet(IO(id, mods, text))

    // Do we have a regulation?
    case reg: BioMention if reg.matches("ComplexEvent") && reg.arguments.contains("controller") && reg.arguments.contains("controlled") =>
      val inputsForAgent = getInputs(findAgent(reg).get)
      val inputsForPatient = getInputs(findPatient(reg).get)
      IOSet(inputsForAgent ++ inputsForPatient)

    // TODO: What is being left out?
    case _ => IOSet.empty
  }

  // Represent the output of a Mention
  // TODO: handle cases with multiple themes?
  def getOutputs(mention: Mention): IOSet = mention match {

    // Use the grounding ID of the TextBoundMention
    // Maybe this one should be removed?
    // Should this only apply if btm.matches("Entity")
    case btm: BioTextBoundMention =>
      val outputs = for (i <- getInputs(mention)) yield IO(i.id, i.mods, i.text)
      IOSet(outputs)

    // Is it an BioEventMention with a theme?
    case bemWithTheme: BioMention if bemWithTheme.matches("SimpleEvent") && bemWithTheme.arguments.contains("theme") =>
      // return the grounding id for the theme
      val m = bemWithTheme.arguments("theme").head
      val id = getGroundingIDasString(m)
      val text = m.text
      val mods =
        // Get event label (PTM)
        Set(bemWithTheme.label) ++
          // Get relevant modifications
          getRelevantModifications(m)
      IOSet(IO(id, mods, text))

    // Do we have a regulation?
    case reg : BioMention if reg.matches("ComplexEvent") && reg.arguments.contains("controller") && reg.arguments.contains("controlled") =>
      val m = reg.arguments("controlled").head
      val id = getGroundingIDasString(m)
      val text = m.text
      val patient = findPatient(m)
      val mods =
        // get top-level event's label (what kind of regulation?)
        Set(reg.label) ++
        // the label of the controlled (PTM?)
        Set(m.label) ++
          // the mods of the theme
          getRelevantModifications(m)
      // the mods of the theme of the theme
      val allMods = if (patient.nonEmpty) getRelevantModifications(patient.get) ++ mods else mods
      IOSet(IO(id, allMods, text))

    // TODO: What is being left out?
    case _ => IOSet.empty
  }

  // Check to see if a Mention has input
  def hasInput(m: Mention): Boolean = getInputs(m).nonEmpty

  // Check to see if a Mention has output
  def hasOutput(m: Mention): Boolean = getOutputs(m).nonEmpty
}
