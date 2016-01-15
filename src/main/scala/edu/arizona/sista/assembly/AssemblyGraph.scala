package edu.arizona.sista.assembly

import edu.arizona.sista.odin.{RelationMention, Mention}


// A Link is an edge connecting two nodes
case class Link(m: RelationMention) {

  // TODO: confirm that mention equality does not involve foundBy attribute
  def reverse:Link = {
    // switch the arguments
    val followers = m.arguments(Architecture.predecessor)
    val predecessors = m.arguments(Architecture.successor)
    // update the old mention's map
    val reversedArgs = m.arguments + (Architecture.predecessor -> predecessors, Architecture.successor -> followers)
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
        val predecessors:Seq[Mention] = m.arguments(Architecture.predecessor)
        val successors:Seq[Mention] = m.arguments(Architecture.successor)
        predecessors.map(p => (p, (Architecture.predecessor, successors))) ++ successors.map(s => (s, (Architecture.successor, predecessors)))
      }).flatten
    // Get a mapping of mention to its incoming mentions & outgoing mentions
    val mentionMap: Map[Mention, Map[String, Seq[Mention]]] = links.groupBy(_._1).mapValues(v => v.map(_._2).toMap)
    val nodes = for {(k,v) <- mentionMap
                     incoming = v.getOrElse (Architecture.predecessor, Nil)
                     outgoing = v.getOrElse (Architecture.successor, Nil)
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
  // TODO: Consider this case:
  // a -> b
  // c -> a
  // should we infer C -> B?
  // or, even given the explicit pairs, does B -> C remain possible in biology?

}

// Defines architecture attributes
object Architecture {
  val predecessor = "before"
  val successor = "after"
}