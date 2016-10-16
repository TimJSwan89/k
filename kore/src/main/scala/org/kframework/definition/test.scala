package org.kframework.definition

import org.kframework.attributes.Att

import org.kframework.kore._


/**
  * Created by lpena on 10/11/16.
  */

class test {

  implicit def BecomingNonTerminal(s: ADT.SortLookup): NonTerminal = NonTerminal(s)
  implicit def BecomingTerminal(s: String): Terminal = Terminal(s)
  implicit def BecomingSequence(ps: ProductionItem*): Seq[ProductionItem] = ps

  import org.kframework.attributes.Att._
  def Sort(s: String): ADT.SortLookup = ADT.SortLookup(s)

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

  // module KTOKENS
  //
  //   token KString       ::= r"\"[a-zA-Z0-9\\-]*\"" [.Attribute]
  //   token KSort         ::= r"[A-Z][A-Za-z0-9]*" [.Attribute]
  //   token KAttributeKey ::= r"[a-z][A-Za-z\\-0-9]*" [.Attribute]
  //   token KModuleName   ::= r"[A-Z][A-Z\\-]*" [.Attribute]
  //
  // endmodule

  val KString = Sort("KString")
  val KSort = Sort("KSort")
  val KAttributeKey = Sort("KAttributeKey")
  val KModuleName = Sort("KModuleName")

  val KTOKENS = Module("KTOKENS", Set(), Set(

    token(KString) is regex("\"[a-zA-Z0-9\\-]*\""),
    token(KSort) is regex("[A-Z][A-Za-z0-9]*"),
    token(KAttributeKey) is regex("[a-z][A-Za-z\\-0-9]*"),
    token(KModuleName) is regex("[A-Z][A-Z\\-]*")

  ))

  // module KATTRIBUTES
  //   imports KTOKENS
  //
  //   syntax KKeyList ::= KAttributeKey [.Attribute]
  //   syntax KKeyList ::= KAttrubitueKey "," KKeyList [.Attribute]
  //
  //   syntax KAttribute ::= KAttributeKey [.Attribute]
  //   syntax KAttribute ::= KAttributeKey "(" KKeyList ")" [.Attribute]
  //   syntax KAttributes ::= KAttribute [.Attribute]
  //   syntax KAttributes ::= KAttribute "," KAttributes [.Attribute]
  //
  // endmodule

  val KKeyList = Sort("KeyList")
  val KAttribute = Sort("Attribute")
  val KAttributes = Sort("Attributes")

  val KATTRIBUTES = Module("KATTRIBUTES", Set(KTOKENS), Set(

    syntax(KKeyList) is KAttributeKey,
    syntax(KKeyList) is (KAttributeKey, ",", KKeyList),

    syntax(KAttribute) is KAttributeKey,
    syntax(KAttribute) is (KAttributeKey, "(", KKeyList, ")"),
    syntax(KAttributes) is ".Attribute",
    syntax(KAttributes) is (KAttribute, ",", KAttributes)

  ))

  // module KSENTENCES
  //   imports KATTRIBUTES
  //
  //   syntax KImport = "imports" KModuleName [.KAttributes]
  //   syntax KImportList = ".KImportList" [.KAttributes]
  //   syntax KImportList = KImport KImportList [.KAttributes]
  //
  //   syntax KTerminal ::= KString [.KAttributes]
  //   syntax KNonKTerminal ::= KSort [.KAttributes]
  //   syntax KProductionItem ::= KTerminal [.KAttributes]
  //   syntax KProductionItem ::= KNonTerminal [.KAttributes]
  //   syntax KProduction ::= KProductionItem [.KAttributes]
  //   syntax KProduction ::= KProductionItem KProduction [.KAttributes]
  //
  //   syntax KPreSentence = "token" KSort "::=" KProduction [.KAttributes]
  //   syntax KPreSentence = "syntax" KSort "::=" KProduction [.KAttributes]
  //   syntax KPreSentence = "axiom" KString [.KAttributes]
  //
  //   syntax KSentence = KPreStentence "[" KAttributes "]" [.KAttributes]
  //   syntax KSentenceList = ".KSentenceList" [.KAttributes]
  //   syntax KSentenceList = KSentence KSentenceList [.KAttributes]
  //
  // endmodule

  val KImport = Sort("KImport")
  val KImportList = Sort("KImportList")

  val KTerminal = Sort("KTerminal")
  val KNonTerminal = Sort("KNonTerminal")
  val KProductionItem = Sort("KProductionItem")
  val KProduction = Sort("KProduction")

  val KPreSentence = Sort("KPreSentence")
  val KSentence = Sort("KSentence")
  val KSentenceList = Sort("KSentenceList")

  val KSENTENCES = Module("KSENTENCES", Set(KATTRIBUTES), Set(
    syntax(KImport) is ("imports", KModuleName),
    syntax(KImportList) is ".KImportList",
    syntax(KImportList) is (KImport, KImportList),

    syntax(KTerminal) is KString,
    syntax(KNonTerminal) is KSort,
    syntax(KProductionItem) is KTerminal,
    syntax(KProductionItem) is KNonTerminal,
    syntax(KProduction) is KProductionItem,
    syntax(KProduction) is (KProductionItem, KProduction),

    syntax(KPreSentence) is ("token", KSort, "::=", KProduction),
    syntax(KPreSentence) is ("syntax", KSort, "::=", KProduction),
    syntax(KPreSentence) is ("axiom", KString),

    syntax(KSentence) is (KPreSentence, "[", KAttributes, "]"),
    syntax(KSentenceList) is (KSentence),
    syntax(KSentenceList) is (KSentence, KSentenceList)
  ))

  // module KDEFINITION
  //   imports KSENTENCES
  //
  //   syntax KModule ::= "module" KModuleName KImportList KSentenceList "endmodule" [.KAttribute]
  //   syntax KModuleList = KModule [.KAttribute]
  //   syntax KModuleList = KModule KModuleList [.KAttribute]
  //
  //   syntax KRequire ::= "require" KString [.KAttribute]
  //   syntax KRequireList ::= ".KRequireList" [.KAttribute]
  //   syntax KRequireList ::= Require RequireList [.KAttribute]
  //
  //   syntax KDefinition ::= KRequireList KModuleList [.KAttribute]
  //
  // endmodule

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KRequire = Sort("KRequire")
  val KRequireList = Sort("KRequireList")
  val KDefinition = Sort("KDefinition")

  val KDEFINITION = Module("KDEFINITION", Set(KSENTENCES), Set(

    syntax(KModule) is ("module", KModuleName, KImportList, KSentenceList, "endmodule"),
    syntax(KModuleList) is KModule,
    syntax(KModuleList) is (KModule, KModuleList),

    syntax(KRequire) is ("require", KString),
    syntax(KRequireList) is ".KRequireList",
    syntax(KRequireList) is (KRequire, KRequireList),

    syntax(KDefinition) is (KRequireList, KModuleList)

  ))

}
