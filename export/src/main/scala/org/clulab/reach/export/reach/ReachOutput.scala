package org.clulab.reach.export.reach

import java.io._
import java.util.Date

import com.typesafe.scalalogging.LazyLogging

// import scala.collection.mutable.{HashMap, ListBuffer, Set => MSet}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization

import org.clulab.reach.assembly.export.{CausalPrecedence, Equivalence}
import org.clulab.reach.assembly.{Assembler, RoleWithFeatures}
import org.clulab.reach.assembly.export.AssemblyLink
import org.clulab.reach.assembly.representations.{PTM => AssemblyPTM}
import org.clulab.odin._
// import org.clulab.processors.Document
import org.clulab.reach.FriesEntry
// import org.clulab.reach.context._
import org.clulab.reach.display._
import org.clulab.reach.export._
import org.clulab.reach.export.JsonOutputter._
import org.clulab.reach.mentions._
import org.clulab.serialization.json
import org.clulab.serialization.json.JSONSerializer.formats
import org.clulab.reach.mentions.serialization.json.{ JSONSerializer => ReachJSONSerializer }
// import org.clulab.reach.serialization.json.{ JSONSerializer => ReachJSONSerializer }


/**
  * Defines classes and methods used to build and output the FRIES format.
  *   Written by: Mihai Surdeanu and Tom Hicks.
  *   Last Modified: Output alternate candidate groundings and grounding species.
  */
class ReachOutput extends JsonOutputter with LazyLogging {

  //
  // Public API:
  //

  /** Returns the given mentions in the Reach JSON format, as one big string. */
  override def toJSON (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    outFilePrefix:String
  ): String = {
    val model:JValue = makeModel(paperId, allMentions, paperPassages, startTime, endTime)
    model.extract[String]                   // REPLACE LATER
    // writeJsonToString(model)
  }


  /**
    * Writes the given mentions to output files in Reach JSON format.
    * Each output file is prefixed with the given prefix string.
    */
  override def writeJSON (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    outFilePrefix:String
  ): Unit = {
    val model:JValue = makeModel(paperId, allMentions, paperPassages, startTime, endTime)
    // writeJsonToFile(model, new File(outFilePrefix + ".reach-json"))
  }


  /**
    * Writes the given mentions to output files in Reach JSON format.
    * Each output file is prefixed with the given prefix string.
    */
  override def writeJSON (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    outFilePrefix:String,
    assemblyApi: Assembler
  ): Unit = {

    val model:JValue = makeModel(paperId, allMentions, paperPassages,
                                  startTime, endTime, Some(assemblyApi))
//    writeJsonToFile(model, new File(outFilePrefix + ".reach-json"))
  }


  /** Make and return the Sentence, Entity, Event, and (optionally) Assembly data models. */
  def makeModel (
    paperId:String,
    allMentions:Seq[Mention],
    paperPassages:Seq[FriesEntry],
    startTime:Date,
    endTime:Date,
    assemblyApi: Option[Assembler] = None
  ): JValue = {
    return JString("Reach JSON format is not yet implemented")
  }

}
