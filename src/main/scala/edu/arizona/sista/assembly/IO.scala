package edu.arizona.sista.assembly

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.reach.mentions._


abstract class IO {
  def id: String
  def mods: Set[String]
  def text: String

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
  def isInputOf(m: Mention): Boolean = this.equals(IOResolver.getInput(m).get)
  // Check to see if this IO is the Output of some Mention
  // Uses custom equality def (text doesn't need to match)
  def isOutputOf(m: Mention): Boolean = this.equals(IOResolver.getOutput(m).get)

  // Check to see if this IO is the Output of some Mention
  // ignores differences in the mods
  def isFuzzyOutputOf(m: Mention): Boolean = {
    val out = IOResolver.getOutput(m).get
    out match {
      case something:Output => this.fuzzyMatch(out)
      case _ => false
    }
  }

  // Check to see if this IO is the Output of some Mention
  // ignores differences in the mods
  def isFuzzyInputOf(m: Mention): Boolean = {
    val input = IOResolver.getInput(m).get
    input match {
      case something:Input => this.fuzzyMatch(input)
      case _ => false
    }
  }
}

case class Input(id: String, mods: Set[String], text: String, into: String) extends IO {

}

case class Output(id: String, mods: Set[String], text: String, from: String) extends IO {

}

object IOResolver {

  def getGroundingIDasString(m: Mention): String = {

    val bm = m.toBioMention
      bm match {
        case tb : BioTextBoundMention => tb.xref.get.printString
        // recursively unpack this guy
        case hasPatient if hasPatient.arguments.contains("controlled") || hasPatient.arguments.contains("theme") =>
          //Seq(hasControlled.label, getGroundingIDasString(hasControlled.arguments("controlled").head)).flatten.mkString("+")
          val patient = if (hasPatient.arguments contains "controlled") hasPatient.arguments("controlled").head else hasPatient.arguments("theme").head
          getGroundingIDasString(patient)
      }
  }

  // Get labels for PTM an Mutant mods
  def getRelevantModifications(m: Mention): Set[String] =
    m.toBioMention.modifications.flatMap {
      case ptm: PTM => Set(ptm.label)
      case mutant: Mutant => Set(mutant.label)
      case _ => Set[String]()
    }

  // Represent the input to a Mention
  // TODO: handle cases with multiple themes?
  def getInput(mention: Mention): Option[Input] = mention match {

    // Use the grounding ID of the TextBoundMention
    // Should this only apply if btm.matches("Entity")
    case btm: BioTextBoundMention =>
      val id = btm.xref.get.printString
      val text = btm.text
      val mods = getRelevantModifications(btm)
      Some(Input(id, mods, text, into = btm.label))

    // Is it an BioEventMention with a theme?
    case bemWithTheme: BioMention if bemWithTheme.matches("SimpleEvent") && bemWithTheme.arguments.contains("theme") =>
      // return the grounding id for the theme
      val m = bemWithTheme.arguments("theme").head
      val id = getGroundingIDasString(m)
      val text = m.text
      val mods =
      // Get theme's label (PTM)
        Set(bemWithTheme.label) ++
          // Get relevant modifications
          getRelevantModifications(m)
      Some(Input(id, mods, text, into = bemWithTheme.label))

    // Do we have a regulation?
    case reg : BioMention if reg.matches("ComplexEvent") && reg.arguments.contains("controller") && reg.arguments.contains("controlled") =>
      val m = reg.arguments("controlled").head
      val id = getGroundingIDasString(m)
      val text = m.text
      val mods =
      // the label of the controlled (PTM?)
        Set(m.label) ++
          // the mods of the theme
          getRelevantModifications(m) ++
          // the mods of the theme of the theme
          getRelevantModifications(m.arguments("theme").head)
      Some(Input(id, mods, text, into = reg.label))

    // TODO: What is being left out?
    case _ => None
  }

  // Represent the output of a Mention
  // TODO: handle cases with multiple themes?
  def getOutput(mention: Mention): Option[Output] = mention match {

    // Use the grounding ID of the TextBoundMention
    // Maybe this one should be removed?
    // Should this only apply if btm.matches("Entity")
    case btm: BioTextBoundMention => {
      getInput(mention) match {
        case input if input.nonEmpty =>
          Some(Output(input.get.id, input.get.mods, input.get.text, from = btm.label))
        case _ => None
      }
    }

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
      Some(Output(id, mods, text, from = bemWithTheme.label))

    // Do we have a regulation?
    case reg : BioMention if reg.matches("ComplexEvent") && reg.arguments.contains("controller") && reg.arguments.contains("controlled") =>
      val m = reg.arguments("controlled").head
      val id = getGroundingIDasString(m)
      val text = m.text
      val mods =
        // get top-level event's label (what kind of regulation?)
        Set(reg.label) ++
        // the label of the controlled (PTM?)
        Set(m.label) ++
          // the mods of the theme
          getRelevantModifications(m) ++
          // the mods of the theme of the theme
          getRelevantModifications(m.arguments("theme").head)
      Some(Output(id, mods, text, from = reg.label))

    // TODO: What is being left out?
    case _ => None
  }

  // Check to see if a Mention has input
  def hasInput(m: Mention): Boolean = getOutput(m).nonEmpty

  // Check to see if a Mention has output
  def hasOutput(m: Mention): Boolean = getOutput(m).nonEmpty
}
