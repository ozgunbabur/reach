package edu.arizona.sista.reach.assembly

import edu.arizona.sista.reach.AssemblyTests
import edu.arizona.sista.reach.TestUtils._
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.assembly.{IO, IOResolver}

class TestAssembly extends FlatSpec with Matchers {

  // test input-output comparisons
  val regtext = "The ubiquitination of Ras is increased by SMAD."
  val mutantRasText = "The ubiquitination of mutant Ras was observed."
  val text = s"$regtext  $mutantRasText"
  val mentions = getBioMentions(text)
  val ubiqs = mentions filter (_ matches "Ubiquitination")
  val regs = mentions.filter(_ matches "Positive_regulation")
  val ubiq1Input = IOResolver.getInput(ubiqs.head).get

  text should "contain 2 ubiquitinations that are NOT exactly equal" taggedAs(AssemblyTests) in {
    // there should be two ubiquitinations
    ubiqs should have size (2)
    // these are not true equivalent entities
    ubiq1Input.isInputOf(ubiqs.last) should not be (true)
  }

  it should "contain 2 ubiquitinations that are APPROXIMATELY equal" taggedAs(AssemblyTests) in {
    ubiq1Input.isFuzzyInputOf(ubiqs.last) should be (true)
  }

  regtext should "take Ubiquitinated Ras (UNMUTATED) as input" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head

    IOResolver.hasInput(reg) should be (true)
    val regInput = IOResolver.getInput(reg).get

    regInput.mods contains "Mutant" should not be (true)

    val nonMutantUbiq:Seq[IO] =
      mentions filter (_ matches "Ubiquitination") flatMap IOResolver.getOutput filterNot(_.mods contains "Mutant")

    nonMutantUbiq should have size (1)

    regInput should equal (nonMutantUbiq.head)
    regInput.fuzzyMatch(nonMutantUbiq.head) should be (true)
  }


  it should "produce Ubiquitinated Ras (UNMUTATED) + Regulation as output" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head

    IOResolver.hasOutput(reg) should be (true)
    val regOutput = IOResolver.getOutput(reg).get

    regOutput.mods contains "Mutant" should not be (true)

    val nonMutantUbiq:Seq[IO] =
      mentions filter (_ matches "Ubiquitination") flatMap IOResolver.getOutput filterNot(_.mods contains "Mutant")

    nonMutantUbiq should have size (1)

    // Should fail because of + Regulation on reg output
    regOutput should not equal (nonMutantUbiq.head)
    // Fuzzy match because of Regulation on regOutput
    regOutput.fuzzyMatch(nonMutantUbiq.head) should be (true)
  }

  // make sure input checks are working
  "IOResolver(m).getInput.get.isInputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getInput(reg).get.isInputOf(reg) should be (true)
  }

  "IOResolver(m).getInput.get.isFuzzyInputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getInput(reg).get.isFuzzyInputOf(reg) should be (true)
  }

  // make sure output checks are working
  "IOResolver(m).getOutput.get.isOutputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getOutput(reg).get.isOutputOf(reg) should be (true)
    IOResolver.getOutput(reg).get.isFuzzyOutputOf(reg) should be (true)
  }

  "IOResolver(m).getOutput.get.isFuzzyOutputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getOutput(reg).get.isFuzzyOutputOf(reg) should be (true)
  }
}
