package edu.arizona.sista.assembly

import java.io.{FileInputStream, FileOutputStream, ObjectOutputStream}

import edu.arizona.sista.odin.Mention
import edu.arizona.sista.utils.ClassLoaderObjectInputStream

/**
 * Serialize / deserialize mentions
 */
object MentionSerializer {
  // see https://github.com/clulab/rule-learning/blob/717cbb5305c8bbb81ea4a838b89b17549b88456a/src/main/scala/org/clulab/bionlp09/ParseDocs.scala

  def save(mentions: Seq[Mention], path: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    oos.writeObject(mentions)
    oos.close()
  }

  def load(path: String): Seq[Mention] = {
    val ois = new ClassLoaderObjectInputStream(getClass.getClassLoader, new FileInputStream(path))
    val data = ois.readObject().asInstanceOf[Seq[Mention]]
    ois.close()
    data
  }

}
