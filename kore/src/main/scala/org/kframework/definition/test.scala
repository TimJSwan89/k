package org.kframework.definition

import org.kframework.attributes.Att

import org.kframework.kore._


/**
  * Created by lpena on 10/11/16.
  */

class test {

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

  implicit def BecomingNonTerminal(s: ADT.SortLookup): NonTerminal = NonTerminal(s)
  implicit def BecomingTerminal(s: String): Terminal = Terminal(s)
  implicit def BecomingSequence(ps: ProductionItem*): Seq[ProductionItem] = ps

  import org.kframework.attributes.Att._
  def Sort(s: String): ADT.SortLookup = ADT.SortLookup(s)

  val Key = Sort("Key")
  val KeyList = Sort("KeyList")
  val Attribute = Sort("Attribute")
  val AttributeList = Sort("AttributeList")
  val Attributes = Sort("Attributes")

  def regex(s: String): ProductionItem = RegexTerminal("", s, "")

  case class token(s: ADT.SortLookup) {
    def is(pis: ProductionItem): BecomingToken = BecomingToken(s, List(pis))
  }

  case class BecomingToken(sort: ADT.SortLookup, pis: Seq[ProductionItem]) {
    def att(atts: String*): Production = Production(sort, pis, atts.foldLeft(Att() + "token")(_+_))
  }

  implicit def tokenWithoutAttributes(bp: BecomingToken) : Production =
    Production(bp.sort, bp.pis, Att() + "token")

  case class syntax(s: ADT.SortLookup) {
    def is(pis: ProductionItem*): BecomingSyntax = BecomingSyntax(s, pis)
  }

  case class BecomingSyntax(sort: ADT.SortLookup, pis: Seq[ProductionItem]) {
    def att(atts: String*): Production = Production(sort, pis, atts.foldLeft(Att())(_+_))
  }

  implicit def syntaxWithoutAttributes(bp: BecomingSyntax) : Production =
    Production(bp.sort, bp.pis, Att())

  case class Axiom(ax: String, attr: Att) extends Sentence {
    val att = attr
  }

  def axiom(ax: String): BecomingAxiom = BecomingAxiom(ax)

  case class BecomingAxiom(ax: String) {
    def att(atts: String*): Axiom = Axiom(ax, atts.foldLeft(Att())(_+_))
  }

  implicit def axiomWithoutAttributes(bax: BecomingAxiom) : Axiom =
    Axiom(bax.ax, Att())

  val ATTRIBUTES = Module("ATTRIBUTES", Set(), Set(
    token(Key) is regex("[a-z][a-zA-Z\\-0-9]*"),
    syntax(KeyList) is Key,
    syntax(KeyList) is Key,
    syntax(KeyList) is (Key, ",", KeyList),
    syntax(Attribute) is Key,
    syntax(Attribute) is (Key, "(", KeyList, ")"),
    syntax(AttributeList) is Attribute,
    syntax(AttributeList) is (Attribute, ",", AttributeList),
    syntax(Attributes) is ("[", AttributeList, "]"),
    axiom("axiom!") att("comm")
    //syntax(KeyList) is (Key, Key) att(assoc, comm, "bag")
  ))

}
