package org.kframework.definition

import org.kframework.attributes.Att

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
    //Production(Int, Syntax(), Att())
  ))

  // module INT
  //   syntax Int
  // endmodule


  import org.kframework.attributes.Att._

  def Att(atts: String*) = atts.foldLeft(org.kframework.attributes.Att())(_+_)

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

  // module ATTRIBUTES
  //   token Key ::= r"[a-z][A-Za-z\\-0-9]*"
  //   syntax KeyList ::= Key
  //   syntax KeyList ::= Key "," KeyList
  //   syntax Attribute ::= Key
  //   syntax Attribute ::= Key "(" KeyList ")"
  //   syntax AttributeList ::= Attribute
  //   syntax AttributeList ::= Attribute "," AttributeList
  //   syntax Attributes ::= "[" AttributeList "]"
  // endmodule

  def Sort(s: String): ADT.SortLookup = ADT.SortLookup(s)

  val Key = Sort("Key")
  val KeyList = Sort("KeyList")
  val Attribute = Sort("Attribute")
  val AttributeList = Sort("AttributeList")
  val Attributes = Sort("Attributes")

  def regex(s: String): RegexTerminal = RegexTerminal("", s, "")

  def Token(s: ADT.SortLookup, re: RegexTerminal): Production =
    Production(s, Syntax(re), Att("token"))

  case class BecomingProduction(sort: ADT.SortLookup, pis: ProductionItem*) {
    def att(atts: String*): Production = Production(sort, pis, atts.foldLeft(Att())(_+_))
  }

  case class syntax(s: ADT.SortLookup) {
    def ::= (pis: ProductionItem*): BecomingProduction = BecomingProduction(s, pis)
  }

  implicit def productionWithoutAttributes(bp: BecomingProduction) : Production =
    Production(bp.sort, bp.pis, Att())

  val ATTRIBUTES = Module("ATTRIBUTES", Set(), Set(
    Token(Key, regex("[a-z][a-zA-Z\\-0-9]*")),
    (syntax(KeyList) ::= (Key)).att("assoc"),
    Production(KeyList, Syntax(Key), Att()),
    Production(KeyList, Syntax(Key, ",", KeyList), Att()),
    Production(Attribute, Syntax(Key), Att()),
    Production(Attribute, Syntax(Key, "(", KeyList, ")"), Att()),
    Production(AttributeList, Syntax(Attribute), Att()),
    Production(AttributeList, Syntax(Attribute, ",", AttributeList), Att()),
    Production(Attributes, Syntax("[", AttributeList, "]"), Att())
  ))

}
