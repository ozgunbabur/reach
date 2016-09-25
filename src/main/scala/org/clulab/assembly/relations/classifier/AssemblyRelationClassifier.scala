package org.clulab.assembly.relations.classifier

import java.io._
import org.clulab.assembly.relations.corpus.EventPair
import org.clulab.learning._
import org.clulab.odin.Mention
import org.clulab.reach.PaperReader
import org.clulab.struct.Counter


class AssemblyRelationClassifier(
  val classifier: Classifier[String, String]
) extends Serializable {

  import AssemblyRelationClassifier._

  val classifierType = classifier.getClass.getCanonicalName

  /** pick the label with the most likely score */
  def classify(datum: RVFDatum[String, String]): String = getLabelScores(datum).argMax._1
  def classify(e1: Mention, e2: Mention): String = classify(mkRVFDatum(UNKNOWN, e1, e2))
  /** return the most likely label and its score */
  def getLabelWithScore(datum: RVFDatum[String, String]): (String, Double) = getLabelScores(datum).argMax
  def getLabelWithScore(e1: Mention, e2: Mention): (String, Double) = getLabelScores(e1, e2).argMax

  /** get the scores for each possible label */
  def getLabelScores(datum: RVFDatum[String, String]): Counter[String] =
    classifier.scoresOf(datum)
  def getLabelScores(e1: Mention, e2: Mention): Counter[String] =
    classifier.scoresOf(mkRVFDatum(UNKNOWN, e1, e2))

  def saveTo(s: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(s))
    oos.writeObject(this)
    oos.close()
  }

}

object AssemblyRelationClassifier {

  val NEG = "None"
  val UNKNOWN = "unknown"
  // reach system (makes use of context parameters specified in config)
  val rs = PaperReader.rs

  // convenience map for plugging in different classifiers
  def getModel(txt: String): Classifier[String, String] = txt match {
    case "lr-l2" => new LogisticRegressionClassifier[String, String]
    case "lr-l1" => new L1LogisticRegressionClassifier[String, String]
    case "lin-svm-l2" => new LinearSVMClassifier[String, String]
    case "lin-svm-l1" => new L1LinearSVMClassifier[String, String]
    case "rf" =>
      def featuresPerNode(total:Int):Int = (total.toDouble * 0.66 ).toInt
      new RFClassifier[String, String](
        numTrees = 10,
        maxTreeDepth = 100,
        howManyFeaturesPerNode = featuresPerNode
      )
    case other => new LogisticRegressionClassifier[String, String]
  }

  /** Takes an RVFDataset and a Classifier[String, String] */
  def train(
    dataset: RVFDataset[String, String],
    clf: Classifier[String, String] = getModel("lin-svm-l2")
  ): AssemblyRelationClassifier = {
    // val clf = new LogisticRegressionClassifier[String, String]
    clf.train(dataset)
    new AssemblyRelationClassifier(clf)
  }

  def loadFrom(p: String): AssemblyRelationClassifier = {
    val path = p.replaceAll("src/main/resources", "")
    // load from resources
    val ois = new ObjectInputStream(getClass.getResourceAsStream(path))
    val clf = ois.readObject.asInstanceOf[AssemblyRelationClassifier]
    ois.close()
    clf
  }

  /** get features for an assembly label corresponding to a pair of mentions */
  def mkRVFDatum(label: String, e1: Mention, e2: Mention): RVFDatum[String, String] =
    FeatureExtractor.mkRVFDatum(e1, e2, label)

  def mkRVFDataset(eps: Seq[EventPair]): RVFDataset[String, String] = {
    val dataset = new RVFDataset[String, String]
    // add each valid annotation to dataset
    for (ep <- eps) {
      val datum = FeatureExtractor.mkRVFDatum(ep.e1, ep.e2, ep.relation)
      dataset += datum
    }
    dataset
  }
}