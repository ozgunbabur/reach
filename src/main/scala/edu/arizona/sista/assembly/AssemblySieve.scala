package edu.arizona.sista.assembly

import edu.arizona.sista.odin._


trait AssemblySieve {

  // the name of the sieve
  def name = this.getClass.getName

  // takes mentions and produces an AssemblyGraph
  def assemble(mentions:Seq[Mention]): AssemblyGraph
}

/**
 * Domain-specific sieve.
 * Check to see if the output of a mention is the input to another
 */
class ExactIOSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    // For each mention, generate (Mention, Input, Output) triples
    // TODO
    AssemblyGraph(Nil, "") // return placeholder
  }
}

/**
 * Domain-specific sieve.
 * Check to see if the output of a mention is the input to another
 */
class ApproximateIOSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    // For each mention, generate (Mention, Input, Output) triples
    // TODO
    AssemblyGraph(Nil, "") // return placeholder
  }
}

/**
 * Linguistic sieve.
 * Run Odin syntactic dep rules to see if mentions are linked by a prep (intra-sentence)
 */
class PrepositionLinkSieve extends AssemblySieve {

  def assemble(mentions:Seq[Mention]): AssemblyGraph = {
    // read rules and initialize state with existing mentions
    // TODO read preps in from config?
    // rule set only produces target RelationMentions
    // TODO
    AssemblyGraph(Nil, "") // return placeholder
  }
}