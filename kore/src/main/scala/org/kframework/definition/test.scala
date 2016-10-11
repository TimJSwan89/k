package org.kframework.definition

import org.kframework.attributes.Att

import scala.collection.mutable.ListBuffer
import org.kframework.kore._


/**
  * Created by lpena on 10/11/16.
  */
class test {

  implicit val builder = ListBuffer[Production]()

  class BecomingSyntax { ??? }

  implicit class SortBecomingSyntax(s: ADT.SortLookup) extends BecomingSyntax {
    def ::= (x: ProductionInfo)(implicit b: ListBuffer[Production]): Unit = { ??? }
  }

  implicit class StringBecomingSyntax(s: String) extends BecomingSyntax { ??? }

  class ProductionInfo {}

  case class syntax(bc: BecomingSyntax*) { ??? }

  implicit class ProductWithAtt(p: syntax) {
    def att(symbols: Att*): ProductionInfo = { ??? }
  }

  val Exp : ADT.SortLookup = ???
  val att1 : Att = ???
  val att2 : Att = ???

  Exp ::= syntax(Exp, "+", Exp) att(att1, att2)  // ok because Strings are StringBecomingSyntax which are BecomingSyntax
  Exp ::= syntax(Exp, Exp)      att()
  //  Exp ::= syntax(Exp, 3, Exp) att()  // rejected by compiler, '3' is not a 'BecomingSyntax'


}
