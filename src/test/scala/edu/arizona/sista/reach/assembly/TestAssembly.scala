package edu.arizona.sista.reach.assembly

import edu.arizona.sista.reach.AssemblyTests
import edu.arizona.sista.reach.TestUtils._
import org.scalatest.{Matchers, FlatSpec}
import edu.arizona.sista.assembly.{IOSet, IO, IOResolver}

class TestAssembly extends FlatSpec with Matchers {


  val text1 = "EGF stimulation leads to tyrosine phosphorylation of Gab1"
  val text1mns = getBioMentions(text1)
  val text1regs = text1mns filter( _ matches "Regulation")

  text1 should "have two inputs" taggedAs(AssemblyTests) in {
    IOResolver.getInputs(text1regs.head) should have size (2)
  }

  it should "have Gab1 (- Phosphorylation) and EGF as input" taggedAs(AssemblyTests) in {
    text1regs should have size (1)
    val gab = text1mns.filter(_.text.equalsIgnoreCase("gab1"))
    val egf = text1mns.filter(_.text.equalsIgnoreCase("egf"))
    gab should have size (1)
    egf should have size (1)
    IOResolver.getOutputs(gab.head).isInputOf(text1regs.head)
    IOResolver.getOutputs(gab.head).isFuzzyInputOf(text1regs.head)
    IOResolver.getOutputs(egf.head).isInputOf(text1regs.head)
    IOResolver.getOutputs(egf.head).isFuzzyInputOf(text1regs.head)
  }

  it should "have 1 output" taggedAs(AssemblyTests) in {
    text1regs should have size (1)
    IOResolver.getOutputs(text1regs.head) should have size (1)
  }

  it should "have Gab1 (+ Phosphorylation) as its output" taggedAs(AssemblyTests) in {
    text1regs should have size (1)
    val gab = text1mns.filter(_.text.equalsIgnoreCase("gab1"))
    // require the + Phos feature
    IOResolver.getOutputs(gab.head).isOutputOf(text1regs.head) should not be (true)
    // fuzzy matching should work
    IOResolver.getOutputs(gab.head).isFuzzyInputOf(text1regs.head)
    val phos = text1mns filter(_ matches "Phosphorylation")
    phos.size should be (1)
    IOResolver.getOutputs(phos.head).isOutputOf(text1regs.head) should be (true)
    IOResolver.getOutputs(text1regs.head).isOutputOf(phos.head) should be (true)
    IOResolver.getOutputs(phos.head).isFuzzyOutputOf(text1regs.head) should be (true)
    IOResolver.getOutputs(text1regs.head).isFuzzyOutputOf(phos.head) should be (true)
    IOResolver.getOutputs(phos.head) == IOResolver.getOutputs(text1regs.head) should be (true)
    IOResolver.getOutputs(phos.head).isFuzzyOutputOf(text1regs.head) should be (true)
  }

  // test input-output comparisons
  val regtext = "The ubiquitination of Ras is increased by SMAD."
  val mutantRasText = "The ubiquitination of mutant Ras was observed."
  val text2 = s"$regtext  $mutantRasText"
  val mentions = getBioMentions(text2)
  val ubiqs = mentions filter (_ matches "Ubiquitination")
  val regs = mentions.filter(_ matches "Positive_regulation")
  val inputsForUbiq = IOResolver.getInputs(ubiqs.head)

  text2 should "contain 2 ubiquitinations that are NOT exactly equal" taggedAs(AssemblyTests) in {
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

  regtext should "have two inputs" taggedAs(AssemblyTests) in {
  }
  // NOTE: used to be "take UBIQUITINATED Ras (UNMUTATED) as input"
  it should "take Ras (- Mutant) and SMAD as inputs" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head

    IOResolver.hasInput(reg) should be (true)
    IOResolver.getInputs(reg) should have size (2)
    val inputsForReg:IOSet = IOResolver.getInputs(reg)

    // The input should not involve a Mutant
    inputsForReg.exists(i => i.mods contains "Mutant" ) should not be (true)

    val nonMutants:Seq[IO] =
      mentions filter (_ matches "Entity") flatMap IOResolver.getOutputs filterNot(_.mods contains "Mutant")

    nonMutants should have size (2)
    val nonMutantRas = nonMutants.filter(_.text.equalsIgnoreCase("ras")).head
    val smad = nonMutants.filter(_.text.equalsIgnoreCase("smad")).head

    val smadPlusRas = IOSet(Seq(smad, nonMutantRas))

    // compare reg input to ras input
    inputsForReg should not equal (nonMutantRas)
    inputsForReg.isFuzzySubsetOf(nonMutantRas) should be (true)
    // reg input should also include smad!
    inputsForReg.isProperFuzzySubsetOf(nonMutantRas) should not be (true)

    // compare reg input to smad input
    inputsForReg should not equal (smad)
    inputsForReg.isFuzzySubsetOf(smad) should be (true)
    // reg input should also include ras!
    inputsForReg.isProperFuzzySubsetOf(smad) should not be (true)

    // compare reg input to smad + Ras outputs
    inputsForReg should equal (smadPlusRas)
    inputsForReg.isFuzzySubsetOf(smadPlusRas) should be (true)
    inputsForReg.isProperFuzzySubsetOf(smadPlusRas) should be (true)

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
        .filterNot(_.mods contains "Mutant")

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
    IOResolver.getInputs(reg).isInputOf(reg) should be (true)
  }

  "IOResolver(m).getInputs.isFuzzyInputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getInputs(reg).isFuzzyInputOf(reg) should be (true)
  }

  // make sure output checks are working
  "IOResolver(m).getOutputs.isOutputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getOutputs(reg).isOutputOf(reg) should be (true)
    IOResolver.getOutputs(reg).isFuzzyOutputOf(reg) should be (true)
  }

  "IOResolver(m).getOutputs.isFuzzyOutputOf(m)" should "be true" taggedAs(AssemblyTests) in {
    val mentions = getBioMentions(regtext)
    val regs = mentions.filter(_ matches "Positive_regulation")
    regs should have size (1)
    val reg = regs.head
    IOResolver.getOutputs(reg).isFuzzyOutputOf(reg) should be (true)
  }
}
