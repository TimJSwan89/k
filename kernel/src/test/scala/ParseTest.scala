package org.kframework.definition

import org.kframework.attributes.Source
import org.kframework.parser.concrete2kore.ParseInModule
import org.junit.Test
import org.junit.Assert._
import org.kframework.kore.ADT.SortLookup
import org.kframework.kore._
import org.kframework.kore.KORE._
import org.kframework.builtin.KLabels.ML_FALSE
import org.kframework.utils.errorsystem.ParseFailedException


class ParseTest {
  import test._

  val expParser = new ParseInModule(EXP)
  val kParser = new ParseInModule(KDEFINITION)

  case class l(s: String) { def apply(args: K*): K = KApply(KLabel(s), args) }
  def k(s: String): K = KApply(KLabel(s))
  def t(st: String, sl: ADT.SortLookup): K = KToken(st, sl)

  def parseTest(parser: ParseInModule, toParse: String, parseAs: SortLookup): K =
    parser.parseString(toParse, parseAs, Source(""))._1 match {
      case Right(x) => x
      case Left(y) => k(ML_FALSE)
    }

  def parseK(toParse: String, parseAs: SortLookup): K = parseTest(kParser, toParse, parseAs)

  @Test def simpExp(): Unit = {
    assertEquals(parseTest(expParser, "0 + 0", Exp), l("_+_")(k("0"), k("0")))
  }

  @Test def ktokens(): Unit = {
    assertEquals(parseK("\"aName0239ntehu\"", KString), t("\"aName0239ntehu\"", KString))
    assertEquals(parseK("SortName", KSort), t("SortName", KSort))
    assertEquals(parseK("klabel", KAttributeKey), t("klabel", KAttributeKey))
    assertEquals(parseK("MYMODULE", KModuleName), t("MYMODULE", KModuleName))
  }

  @Test def kml(): Unit = {
    println(kParser.parseString("kmlvar(\"testVar\")", KMLVar, Source("")))
    println(kParser.parseString("KMLtrue", KMLFormula, Source("")))
    println(kParser.parseString("KMLfalse", KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(\"testVar\") KMLand KMLtrue", KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(\"testVar\") KMLor KMLfalse", KMLFormula, Source("")))
    println(kParser.parseString("KMLnot kmlvar(\"testVar\")", KMLFormula, Source("")))
    println(kParser.parseString("KMLexists kmlvar(\"testVar\") . KMLtrue", KMLFormula, Source("")))
    println(kParser.parseString("KMLforall kmlvar(\"testVar\") . KMLtrue", KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(\"testVar\") KML=> KMLtrue", KMLFormula, Source("")))
    assertTrue(true)
  }

}
