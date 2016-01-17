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
  val inputsForUbiq = IOResolver.getInputs(ubiqs.head)

  text should "contain 2 ubiquitinations that are NOT exactly equal" taggedAs(AssemblyTests) in {
    // there should be two ubiquitinations
    ubiqs should have size (2)
    // these are not true equivalent entities
    IOResolver.getInputs(ubiqs.head) == IOResolver.getInputs(ubiqs.last) should not be (true)
  }

  it should "contain 2 ubiquitinations that are APPROXIMATELY equal" taggedAs(AssemblyTests) in {
    // there should be two ubiquitinations
    ubiqs should have size (2)
    IOResolver.getInputs(ubiqs.head).isProperFuzzySubsetOf(IOResolver.getInputs(ubiqs.last)) should be (true)
  }

  // NOTE: used to be "take UBIQUITINATED Ras (UNMUTATED) as input"
  regtext should "take Ras (UNMUTATED) as input" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head

    IOResolver.hasInput(reg) should be (true)
    val inputsForReg = IOResolver.getInputs(reg)

    // The input should not involve a Mutant
    inputsForReg.exists(i => i.mods contains "Mutant" ) should not be (true)

    val nonMutants:Seq[IO] =
      mentions filter (_ matches "Entity") flatMap IOResolver.getOutputs filterNot(_.mods contains "Mutant")

    nonMutants should have size (2)
    val nonMutantRas = nonMutants.filter(_.text.equalsIgnoreCase("Ras")).head
    inputsForReg should equal (nonMutantRas)
    inputsForReg.isProperFuzzySubsetOf(nonMutantRas) should be (true)
  }


  it should "produce Ubiquitinated Ras (UNMUTATED) as output" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head

    IOResolver.hasOutput(reg) should be (true)
    val outputsForReg = IOResolver.getOutputs(reg)

    outputsForReg.exists(_.mods contains "Mutant") should not be (true)

    val nonMutants:Seq[IO] =
      mentions
        .filter(_ matches "Entity")
        .flatMap(IOResolver.getOutputs)
        .filterNot(m => (m.mods contains "Mutant"))

    nonMutants should have size (2)

    val nonMutantRas = nonMutants.filter(_.text.equalsIgnoreCase("Ras")).head
    // Should fail because input is now considered to be Ras (NOT Ras + Ubiq.)
    outputsForReg should not equal (nonMutantRas)
    // Fuzzy match because of Regulation on regOutput
    outputsForReg.exists(_.fuzzyMatch(nonMutantRas)) should be (true)
  }

  // make sure input checks are working
  "IOResolver(m).getInputs.isInputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getInputs(reg).exists(_.isInputOf(reg)) should be (true)
  }

  "IOResolver(m).getInputs.isFuzzyInputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getInputs(reg).exists(_.isFuzzyInputOf(reg)) should be (true)
  }

  // make sure output checks are working
  "IOResolver(m).getOutputs.get.isOutputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getOutputs(reg).exists(_.isOutputOf(reg)) should be (true)
    IOResolver.getOutputs(reg).exists(_.isFuzzyOutputOf(reg)) should be (true)
  }

  "IOResolver(m).getOutputs.get.isFuzzyOutputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getOutputs(reg).exists(_.isFuzzyOutputOf(reg)) should be (true)
  }
}
