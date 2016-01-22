package edu.arizona.sista.assembly

import java.io.{PrintWriter, File}
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.ReachSystem
import edu.arizona.sista.reach.mentions.BioMention
import edu.arizona.sista.reach.nxml.NxmlReader
import edu.arizona.sista.reach.mentions._
import scala.util.{Try, Success, Failure}

// Ugh...this needs to be refactored...
class SieveBasedAssembler(sieves: Seq[AssemblySieve]) {

  // Take a seq of AssemblySieve and return a Seq[AssemblyGraph]
  def assembleAndFilter(mentions:Seq[Mention], s:Seq[AssemblySieve]):Seq[AssemblyGraph] = s.map(_.assembleAndFilter(mentions))
  // Take a seq of AssemblySieve and return a Seq[AssemblyGraph]
  def assembleAndFilter(mentions:Seq[Mention]):Seq[AssemblyGraph] = sieves.map(_.assembleAndFilter(mentions))

  // Take a seq of AssemblySieve and return a Seq[AssemblyGraph]
  def assemble(mentions:Seq[Mention], s:Seq[AssemblySieve]):Seq[AssemblyGraph] = s.map(_.assemble(mentions))
  // Take a seq of AssemblySieve and return a Seq[AssemblyGraph]
  def assemble(mentions:Seq[Mention]):Seq[AssemblyGraph] = sieves.map(_.assemble(mentions))

  // Group Assembly RelationMentions by ("before" arg's output, "after" arg's ouputs)
  // return a Seq[(distinct assembly RM, count of evidence)]
  def group(am: Seq[RelationMention]):Seq[(RelationMention, Int)]= {
    def simplifyIOforGrouping(ioset:IOSet):Set[(String, Set[String])] = ioset.map(io => (io.id, io.mods))
    // We need some way to pick a representative assembly mention for the group
    // One dumb solution is summing the lengths of the "before" and "after" text (i.e. choosing the shortest)
    // The assumption is the shortest string reps should be the easiest to identify as canonical cases of the link
    def chooseShortest(ams: Seq[RelationMention]):RelationMention = {
      ams.minBy(am => {
        val before = am.arguments("before").head
        val after = am.arguments("after").head

        (before.text.length, after.text.length)
      })
    }
   am
     .groupBy(m => (simplifyIOforGrouping(IOResolver.getOutputs(m.arguments("before").head)), simplifyIOforGrouping(IOResolver.getOutputs(m.arguments("after").head)) ))
     .values
     .map(v => (chooseShortest(v), v.size))
     .toSeq
  }

  def displayAssembledMention(m: Mention):Unit = println(generateAssembledMentionRepresentation(m, evidence = 0))
  def displayAssembledMention(m: Mention, seen: Int):Unit = println(generateAssembledMentionRepresentation(m, evidence = seen))

  def generateAssembledMentionRepresentation(m: Mention, evidence:Int = 0):String = {
    val before = m.arguments("before").head
    val after = m.arguments("after").head
    val text =
      if ((before.document != after.document) || (before.sentence != after.sentence))
        // TODO: Add sentence window to L & R of before and after
        s"${before.text} ... ${after.text}"
      else m.text
   val representation = evidence match {
     case 0 =>
       s"""mention:             ${m.label}
          |foundBy:             ${m.foundBy}
          |text:                $text
          |"before" input:      ${IOResolver.getInputs(before)}
          |"before" output:     ${IOResolver.getOutputs(before)}
          |"before" text:       ${before.text}
          |"before" label:      ${before.label}
          |"before" doc id:     ${before.document.id.get}
          |"after" input:       ${IOResolver.getInputs(after)}
          |"after" output:      ${IOResolver.getOutputs(after)}
          |"after" text:        ${after.text}
          |"after" label:       ${after.label}
          |"after" doc id:      ${after.document.id.get}
          |""".stripMargin
     case _ =>
        s"""mention:             ${m.label}
           |evidence:            $evidence
           |foundBy:             ${m.foundBy}
           |text:                $text
           |"before" input:      ${IOResolver.getInputs(before)}
           |"before" output:     ${IOResolver.getOutputs(before)}
           |"before" text:       ${before.text}
           |"before" label:      ${before.label}
           |"before" doc id:     ${before.document.id.get}
           |"after" input:       ${IOResolver.getInputs(after)}
           |"after" output:      ${IOResolver.getOutputs(after)}
           |"after" text:        ${after.text}
           |"after" label:       ${after.label}
           |"after" doc id:      ${after.document.id.get}
           |""".stripMargin
   }
    representation
}

  def groupAndDisplay(ams: Seq[RelationMention]):Unit = {
    // descending sort of groups by amount of evidence found
    group(ams).sortWith(_._2 > _._2).foreach(pair => displayAssembledMention(pair._1, pair._2))
  }

  def groupAssemblyGraphsAndWriteToTSV(ags:Seq[AssemblyGraph], outFile: String):Unit = {
    // consolidate and count
    // TODO: use this after sieves have been evaluated
    //val ams = group(ags.flatMap(_.connected))
    // group within each sieve
    // While duplicates may exist across sieves,
    // this approach is needed to evaluate precision per sieve
    val ams:Seq[(RelationMention, Int)] = ags.flatMap(ag => group(ag.connected)).distinct
    val header = Seq(
      "Label", "Sieve", "Times assembled", "Text", "Correct?",
      "BEFORE input", "BEFORE output", "BEFORE text", "BEFORE label", "BEFORE doc id",
      "AFTER input", "AFTER output", "AFTER text", "AFTER label", "AFTER doc id",
      "Notes"
    ).mkString("\t")
    val lines =
      for {
        (am, evidence) <- ams
        before = am.arguments("before").head
        after = am.arguments("after").head
        // TODO: Add sentence window to L & R of before and after
        text = if ((before.document != after.document) || (before.sentence != after.sentence)) s"${before.text} ... ${after.text}" else am.text
    } yield Seq(
        am.label, am.foundBy, evidence, text, "",
        IOResolver.getInputs(before), IOResolver.getOutputs(before), before.text, before.label, before.document.id.get,
        IOResolver.getInputs(after), IOResolver.getOutputs(after), after.text, after.label, after.document.id.get,
        ""
      ).mkString("\t")
      // write results to file
      val f = new File(outFile)
      val pw = new PrintWriter(f)
      pw.write((Seq(header) ++ lines).mkString("\n"))
      pw.close()
  }
}

object SieveBasedAssemblySystem extends App {

  val sieves = Seq(new ExactIOSieve, new ApproximateIOSieve, new PrepositionLinkSieve)
  val assembler = new SieveBasedAssembler(sieves)

  val reader = new NxmlReader
  // Initialize the REACH system
  val rs = new ReachSystem

  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  // read from config
  val serializeMentionsPath = config.getString("assembly.mentions.serializeto")
  val mentions:Seq[BioMention] =
    // attempt to load the serialized mentions
    Try(MentionSerializer.load(serializeMentionsPath)) match {
      // did we successfully load the serialized mentions?
      case Success(preProcessedMentions) => preProcessedMentions.map(_.toBioMention)
      case Failure(oops) => {
        val nxmlDir = new File(config.getString("nxmlDir"))
        // Extract mentions
        val mentionsAcrossPapers =
          for { p <- nxmlDir.listFiles.par
                entries = reader.readNxml(p) } yield entries flatMap rs.extractFrom
        mentionsAcrossPapers.flatten.toVector.toSeq
      }
    }
  // now we assemble...

  def assembleWithinPaper(mentions: Seq[Mention], sieves:Seq[AssemblySieve]):Seq[AssemblyGraph] = {
    // placeholder
    assembler.assemble(mentions, sieves)
  }


}
