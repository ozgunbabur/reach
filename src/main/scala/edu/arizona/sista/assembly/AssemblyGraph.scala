package edu.arizona.sista.assembly

import edu.arizona.sista.odin.{RelationMention, Mention}


// A Link is an edge connecting two nodes
case class Link(m: RelationMention) {

  // TODO: confirm that mention equality does not involve foundBy attribute
  def reverse:Link = {
    // switch the arguments
    val followers = m.arguments("input")
    val predecessors = m.arguments("output")
    // update the old mention's map
    val reversedArgs = m.arguments + ("input" -> predecessors, "output" -> followers)
    Link(m.copy(arguments = reversedArgs))
  }
}


case class Node(node:Mention, incoming: Seq[Mention], outgoing:Seq[Mention])

// A partial graph of assembled mentions
case class AssemblyGraph(connected:Seq[RelationMention], producedBy:String) {

  // used for consistency checks
  val links:Seq[Link] = connected.map(Link)

  // produce a graph-like map encoding reachability from mention -> mention
  val nodes:Seq[Node] = {
    val links:Seq[(Mention, (String, Seq[Mention]))] =
      (for (m <- connected) yield {
        // RelationMentions used in the AssemblyGraph should contain the keywords "input" and "output"
        val predecessors:Seq[Mention] = m.arguments("input")
        val successors:Seq[Mention] = m.arguments("output")
        predecessors.map(p => (p, ("input", successors))) ++ successors.map(s => (s, ("output", predecessors)))
      }).flatten
    // Get a mapping of mention to its incoming mentions & outgoing mentions
    val mentionMap: Map[Mention, Map[String, Seq[Mention]]] = links.groupBy(_._1).mapValues(v => v.map(_._2).toMap)
    val nodes = for {(k,v) <- mentionMap
                     incoming = v.getOrElse ("input", Nil)
                     outgoing = v.getOrElse ("output", Nil)
    } yield Node(k, incoming, outgoing)
    nodes.toSeq
  }

  // To be used with precision-ranked sieves
  // (assumes current AssemblyGraph Precision > g2 Precision)
  def imposeConsistencyConstraint(g2: AssemblyGraph):AssemblyGraph = {
    // create sequences of Link for easier checking of member consistency
    val graph1:Seq[Link] = this.links
    val graph2:Seq[Link] = g2.links
    // filter out any members of graph2 that are inconsistent with graph1
    val graph2Prime = graph2.filterNot(consistent => graph1 contains consistent.reverse)
    // return union of graph1 and constrained graph2
    AssemblyGraph((graph1.map(link => link.m) ++ graph2Prime.map(_.m)).distinct, g2.producedBy)
  }

}
