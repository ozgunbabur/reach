package org.clulab.reach.assembly.relations

import com.typesafe.config.ConfigFactory
import org.clulab.reach.assembly.relations.corpus.{Corpus, CorpusReader, EventPair}
import org.clulab.odin._
import org.clulab.reach.assembly.{AssemblyManager, PrecedenceRelation}
import org.clulab.reach.assembly.sieves._
import SieveUtils._
import ai.lum.common.FileUtils._
import com.typesafe.scalalogging.LazyLogging
import java.io.File


object SieveEvaluator {

  case class Performance (sieve: String, rule: String, p: Double, r: Double, f1: Double, tp: Int, fp: Int, fn: Int) {
    def mkRow = f"$sieve\t$rule\t$p%1.3f\t$r%1.3f\t$f1%1.3f\t$tp\t$fp\t$fn"
  }

  // see RunAnnotationEval

  val config = ConfigFactory.load()
  lazy val eps: Seq[EventPair] = CorpusReader.readCorpus(config.getString("assembly.corpus.corpusDir")).instances

  implicit class EventPairOps(ep: EventPair) {

    def eventPairToEvidence: Set[Mention] = ep match {

      case prec if SieveUtils.precedenceRelations contains prec.relation =>
        val precedenceMentionLabels = Seq(SieveUtils.precedenceMentionLabel)

        val args = prec.relation match {
          case `E1PrecedesE2` =>
            Map[String, Seq[Mention]](SieveUtils.beforeRole -> Seq(ep.e1), SieveUtils.afterRole -> Seq(ep.e2))
          case `E2PrecedesE1` =>
            Map[String, Seq[Mention]](SieveUtils.beforeRole -> Seq(ep.e2), SieveUtils.afterRole -> Seq(ep.e1))
        }

        val mention = ep.e1.sentence == ep.e2.sentence match {

          case true => new RelationMention(
            precedenceMentionLabels,
            args,
            ep.e1.sentence,
            ep.e1.document,
            true,
            "<MANUAL>"
          )

          case false =>
            new CrossSentenceMention(
              labels = precedenceMentionLabels,
              anchor = ep.e1,
              neighbor = ep.e2,
              arguments = args,
              document = ep.e1.document,
              keep = true,
              foundBy = "<MANUAL>"
          )
      }
        Set(mention)

      case _ => Set.empty
    }
  }

  implicit class CorpusOps(corpus: Corpus) {

    def precedenceRelations: Seq[PrecedenceRelation] = for {
      ep <- corpus.instances
      e1 = ep.e1
      e2 = ep.e2
      // is this a precedence relation?
      if SieveUtils.precedenceRelations contains ep.relation
    } yield {
      val am = AssemblyManager()
      am.trackMentions(Seq(e1, e2))

      ep.relation match {
        case `E1PrecedesE2` =>
          PrecedenceRelation(am.getEER(e1), am.getEER(e2), Set.empty[Mention], "gold")
        case `E2PrecedesE1` =>
          PrecedenceRelation(am.getEER(e2), am.getEER(e1), Set.empty[Mention], "gold")
      }
    }

    def filterPRsByText(text: String): Seq[(String, String, PrecedenceRelation)] = precedenceRelations
      .filter(_.before.sourceMention.get.sentenceObj.getSentenceText contains text)
      // get (before's label, after's label, and PR)
      .map(pr => (pr.before.sourceMention.get.label, pr.after.sourceMention.get.label, pr))
  }

  /**
    * Applies each Assembly Sieve to mentions and returns and updated AssemblyManager for each.
    *
    * @param mentions a Seq of Odin Mentions
    * @return an AssemblyManager
    */
  def applyEachSieve(mentions: Seq[Mention]): Map[String, AssemblyManager] = {

    val dedup = new DeduplicationSieves()
    val precedence = new PrecedenceSieves()

    val availableSieves: Map[String, AssemblySieve] = Map(
      //"reichenbachPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.reichenbachPrecedence)),
      //"intrasententialRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.intrasententialRBPrecedence)),
      //"intersententialRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.intersententialRBPrecedence)),
      "discourseRBPrecedence" -> (AssemblySieve(dedup.trackMentions) andThen AssemblySieve(precedence.discourseRBPrecedence))
    )

    val ams = for {
      (lbl, s) <- availableSieves.par
      am = s.apply(mentions)
    } yield lbl -> am

    ams.seq //++ Map("all" -> applySieves(mentions))
  }


  def summarizePrecedenceRelations(pr: Seq[PrecedenceRelation]): String = {
    val crossSentenceCount = pr.count(pr => pr.before.sourceMention.get.sentence != pr.after.sourceMention.get.sentence)
    val eventPairs = pr.map{ pr =>
      val before = pr.before.sourceMention
      val after = pr.after.sourceMention

      (before, after) match {

        case (Some(b), Some(a)) =>
          val rm = AssemblyActions.mkPrecedenceMention(b, a, pr.evidence.toSeq.map(_.foundBy).mkString(", "))
          AssemblyActions.summarizeBeforeAfter(rm)

        case other =>
          s"""NO EVIDENCE FOUND
              |${pr.before.toString}
              |${pr.after.toString}
          """.stripMargin
      }
    }.mkString("\n")

    s"""TOTAL:\t${pr.size}
       |-------------------------------------
       |# CROSS-SENTENCE:\t$crossSentenceCount
       |# INTRASENTENCE:\t${pr.size - crossSentenceCount}
       |
       |$eventPairs
     """.stripMargin
  }

  def evaluateSieves(posGold: Seq[PrecedenceRelation], results: Map[String, AssemblyManager]): Seq[Performance] = {

    println("sieve\trule\tp\tr\tf1\ttp\tfp\tfn")

    val performanceOfEachSieve = for {
      (lbl, sieveResult) <- results
    } yield {
      val predicted = sieveResult.getPrecedenceRelations
      val smoothing = 0.00001
      val im = false
      // get the sets of PrecedenceRelations corresponding to tp, fp, and fn
      val tp = predicted.filter(p => posGold exists(g => g.isEquivalentTo(p, ignoreMods = im)))
      val fp = predicted.filter(p => ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = im)))
      val fn = posGold.filter(g => ! predicted.exists(p => p.isEquivalentTo(g, ignoreMods = im)))

      // micro performance
      val p = tp.size / (tp.size + fp.size + smoothing)
      val r = tp.size / (tp.size + fn.size + smoothing)
      val f1 = (2 * p * r) / (p + r + smoothing)

      // for the whole sieve
      val sievePerformance = Performance(lbl, "**ALL**", p, r, f1, tp.size, fp.size, fn.size)

      // write true positives, false positives, and false negatives to files
      new File(s"true-positives.txt").writeString(summarizePrecedenceRelations(tp.toSeq), java.nio.charset.StandardCharsets.UTF_8)
      new File(s"false-positives.txt").writeString(summarizePrecedenceRelations(fp.toSeq), java.nio.charset.StandardCharsets.UTF_8)
      new File(s"false-negatives.txt").writeString(summarizePrecedenceRelations(fn), java.nio.charset.StandardCharsets.UTF_8)

      val rulePerformance: Seq[Performance] = {
        val rulePs = predicted.groupBy(pr => (pr.foundBy, pr.evidence.head.foundBy))
        val allRtp = rulePs.mapValues(_.count(p => posGold exists(g => g.isEquivalentTo(p, ignoreMods = im))))
        val allRfp = rulePs.mapValues{_.count{p =>
          val isFP = ! posGold.exists(g => g.isEquivalentTo(p, ignoreMods = im))
          //if(isFP) displayMention(p.evidence.head)
          isFP
        }
        }
        val allRfn = {
          val res = for {
            (foundBy, group) <- rulePs
            gold = posGold.count(g => ! group.exists(p => p.isEquivalentTo(g, ignoreMods = im)))
          } yield (foundBy, gold)
          res
        }

        val rp = for {
          foundBy <- rulePs.keys
        } yield {
          val tp = allRtp.getOrElse(foundBy, 0)
          val fp = allRfp.getOrElse(foundBy, 0)
          val fn = allRfn.getOrElse(foundBy, 0)

          // micro performance
          val p = tp / (tp + fp + smoothing)
          val r = tp / (tp + fn + smoothing)
          val f1 = (2 * p * r) / (p + r + smoothing)

          // for the rule
          Performance (foundBy._1, foundBy._2, p, r, f1, tp, fp, fn)
        }
        rp.toSeq
      }

      (rulePerformance.sortBy(_.rule) :+ sievePerformance).foreach(perf => println(perf.mkRow))
      println()
      sievePerformance
    }

    performanceOfEachSieve.toSeq
  }
}

object RunRBSieveEval extends App with LazyLogging {

  import SieveEvaluator._
  import ai.lum.common.ConfigUtils._

  val config = ConfigFactory.load()
  val corpusDir: String = config[String]("assembly.corpus.corpusDir")
  logger.info(s"Reading corpus from $corpusDir")
  val corpus = Corpus(corpusDir)
  logger.info("Applying sieves")
  val sieveResults = SieveEvaluator.applyEachSieve(corpus.mentions)
  val preformanceForEachSieve = SieveEvaluator.evaluateSieves(corpus.precedenceRelations, sieveResults)

}
