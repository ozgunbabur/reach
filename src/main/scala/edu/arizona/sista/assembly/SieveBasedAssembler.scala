package edu.arizona.sista.assembly

import java.io.File
import com.typesafe.config.ConfigFactory
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.ReachSystem
import edu.arizona.sista.reach.mentions.BioMention
import edu.arizona.sista.reach.nxml.NxmlReader
import edu.arizona.sista.reach.mentions._
import scala.util.{Try, Success, Failure}

class SieveBasedAssembler {

  // Take a seq of AssemblySieve and return a Seq[AssemblyGraph
  def assemble(mentions:Seq[Mention], sieves:Seq[AssemblySieve]):Seq[AssemblyGraph] = sieves.map(_.assemble(mentions))

}

object SieveBasedAssemblySystem extends App {

  val assembler = new SieveBasedAssembler()

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
