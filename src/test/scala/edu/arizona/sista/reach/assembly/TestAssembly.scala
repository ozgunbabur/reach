package edu.arizona.sista.reach.assembly

import edu.arizona.sista.reach.TestUtils._
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.assembly.IOResolver

class TestAssembly extends FlatSpec with Matchers {

  // test input-output comparisons
  val regtext = "The ubiquitination of Ras is increased by SMAD."
  val mutantRasText = "The ubiquitination of mutant Ras was observed."
  val text = s"$regtext  $mutantRasText"
  val mentions = getBioMentions(text)
  val ubiqs = mentions filter (_ matches "Ubiquitination")
  val regs = mentions.filter(_ matches "Positive_regulation")
  val ubiq1Input = IOResolver.getInput(ubiqs.head).get

  text should "contain 2 ubiquitinations that are NOT exactly equal" in {
    // there should be two ubiquitinations
    ubiqs should have size (2)
    // these are not true equivalent entities
    ubiq1Input.isInputOf(ubiqs.last) should not be (true)
  }

  text should "contain 2 ubiquitinations that are APPROXIMATELY equal" in {
    ubiq1Input.isFuzzyInputOf(ubiqs.last) should be (true)
  }

  regtext should "take Ubiquitinated Ras (UNMUTATED) as input" in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head

    IOResolver.hasInput(reg) should be (true)
    val regInput = IOResolver.getInput(reg).get

    regInput.mods contains "Mutant" should not be (true)

    val nonMutantUbiq = mentions filter (_ matches "Ubiquitination") flatMap IOResolver.getOutput filterNot(_.mods contains "Mutant")

    nonMutantUbiq should have size (1)

    regInput == nonMutantUbiq.head should be (true)
  }

  regtext should "produce Ubiquitinated Ras (UNMUTATED) as output" in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head

    IOResolver.hasOutput(reg) should be (true)
    val regOutput = IOResolver.getOutput(reg).get

    regOutput.mods contains "Mutant" should not be (true)

    val nonMutantUbiq = mentions filter (_ matches "Ubiquitination") flatMap IOResolver.getOutput filterNot(_.mods contains "Mutant")

    nonMutantUbiq should have size (1)

    regOutput == nonMutantUbiq.head should be (true)
  }
}
