package org.kframework.definition

import scala.collection.mutable.ListBuffer
import org.kframework.kore._


/**
  * Created by lpena on 10/11/16.
  */

class test {

  implicit def BecomingNonTerminal(s: ADT.SortLookup): NonTerminal = NonTerminal(s)
  implicit def BecomingTerminal(s: String): Terminal = Terminal(s)
  def Syntax(ps: ProductionItem*) = ps

  val Int = ADT.SortLookup("Int")
  val Exp = ADT.SortLookup("Exp")
  val INT = Module("INT", Set(), Set(
    SyntaxSort(Int)
  ))

  // module INT
  //   syntax INTe
  // endmodule

  def Att(atts: String*) = atts.foldLeft(org.kframework.attributes.Att())(_+_)
  import org.kframework.attributes.Att._

  val IMP = Module("IMP", Set(INT), Set(
    Production(Exp, Syntax(Int, "+", Int), Att()),
    Production(Exp, Syntax(Int, "*", Int), Att(assoc, "comm", bag))
  ))

  // Production(Exp, Seq[ProductionItem](NonTerminal(Int), Terminal("+"), NonTerminal(Int)), Att())

  // module IMP
  //   imports INT
  //   syntax Exp ::= Int "+" Int
  //   syntax Exp ::= Int "*" Int
  // endmodule

}
