import scala.util._
import scala.util.parsing.combinator._
import java.io.FileReader

import OurObjects._

object ImpParser extends RegexParsers {
  /* General helpers. */
  def pvar  : Parser[String] = "\\p{Alpha}(\\p{Alnum}|_)*".r

  /* Parsing for ArithExp. */
  def num   : Parser[ArithExp] = "-?\\d+".r ^^ (s => Num(s.toInt))
  def atom  : Parser[ArithExp] =
    "(" ~> aexp <~ ")" |
    pvar ~ ("[" ~> aexp <~ "]") ^^ {case v ~ i => Read(v, i)} |
    num | pvar ^^ { Var(_) } |
    "-" ~> atom ^^ { Sub(Num(0), _) }
  def factor: Parser[ArithExp] =
    atom ~ rep("*" ~ atom | "/" ~ atom | "%" ~ atom) ^^ {
      case left ~ list => (left /: list) {
        case (left, "*" ~ right) => Mul(left, right)
        case (left, "/" ~ right) => Div(left, right)
        case (left, "%" ~ right) => Mod(left, right)
      }
    }
  def term  : Parser[ArithExp] =
    factor ~ rep("+" ~ factor | "-" ~ factor) ^^ {
      case left ~ list => (left /: list) {
        case (left, "+" ~ right) => Add(left, right)
        case (left, "-" ~ right) => Sub(left, right)
      }
    }
  def aexp  : Parser[ArithExp] = term

  /* Parsing for Comparison. */
  def comp  : Parser[Comparison] =
    aexp ~ ("=" | "<=" | ">=" | "<" | ">" | "!=") ~ aexp ^^ {
      case left ~ op ~ right => (left, op, right)
    }

  /* Parsing for BoolExp. */
  def batom : Parser[BoolExp] =
    "(" ~> bexp <~ ")" | comp ^^ { BCmp(_) } | "!" ~> batom ^^ { BNot(_) }
  def bconj : Parser[BoolExp] =
    batom ~ rep("&&" ~> batom) ^^ {
      case left ~ list => (left /: list) { BConj(_, _) }
    }
  def bdisj : Parser[BoolExp] =
    bconj ~ rep("||" ~> bconj) ^^ {
      case left ~ list => (left /: list) { BDisj(_, _) }
    }
  def bexp  : Parser[BoolExp] = bdisj

  /* Parsing for Assertion. */
  def antatom : Parser[Assertion] =
    bexp ^^ { Assn(_) } |
    ("forall" ~> pvar <~ ",") ~ assn ^^ {
      case v ~ a => ForAll(v, a)
    } |
    ("exists" ~> pvar <~ ",") ~ assn ^^ {
      case v ~ a => Exists(v, a)
    }
  def acmp : Parser[Assertion] = 
    antatom | "(" ~> antatom <~ ")" | "!" ~> antatom ^^ { ANot(_) }
  def aimp : Parser[Assertion] =
    acmp ~ rep("==>" ~> acmp) ^^ {
      case left ~ list => (left /: list) {AImp(_, _)}
    }
  def aconj : Parser[Assertion] =
    aimp ~ rep("&&" ~> aimp) ^^ {
      case left ~ list => (left /: list) { AConj(_, _) }
    }
  def adisj : Parser[Assertion] =
    aconj ~ rep("||" ~> aconj) ^^ {
      case left ~ list => (left /: list) { ADisj(_, _) } 
    }
  def assn : Parser[Assertion] = adisj

  /* Parsing for Postcondition Assertions. */
  def postblock : Parser[AssertionBlock] = rep(postantn)
  def postantn : Parser[Assertion] =
    ("post" ~> assn)
    // ("post" ~> assn) ^^ {
    //   case a => Post(a) 
    // }

  /* Parsing for Precondition Assertions. */
  def preblock : Parser[AssertionBlock] = rep(preantn)
  def preantn : Parser[Assertion] =
    ("pre" ~> assn)
    // ("pre" ~> assn) ^^ {
    //   case a => Pre(a)
    // }

  /* Parsing for Invariant Assertions. */
  def invblock : Parser[AssertionBlock] = rep(invantn)
  def invantn : Parser[Assertion] =
    ("inv" ~> assn)
    // ("inv" ~> assn) ^^ {
    //   case a => Inv(a)
    // }

  /* Parsing for Statement and Block. */
  def block : Parser[Block] = rep(stmt)
  def stmt  : Parser[Statement] =
    pvar ~ ("[" ~> aexp <~ "]") ~ (":=" ~> aexp <~ ";") ^^ {
      case v ~ i ~ e => Write(v, i, e)
    } |
    (pvar <~ ":=") ~ (aexp <~ ";") ^^ {
      case v ~ e => Assign(v, e)
    } |
    (pvar <~ ",") ~ (pvar <~ ":=") ~ (aexp <~ ",") ~ (aexp <~ ";") ^^ {
      case v1 ~ v2 ~ e1 ~ e2 => ParAssign(v1, v2, e1, e2)
    } |
    ("if" ~> bexp <~ "then") ~ (block <~ "else") ~ (block <~ "end") ^^ {
      case c ~ t ~ e => If(c, t, e)
    } |
    ("if" ~> bexp <~ "then") ~ (block <~ "end") ^^ {
      case c ~ t => If(c, t, Nil)
    } |
    ("while" ~> bexp) ~ invblock ~ ("do" ~> block <~ "end") ^^ {
      case c ~ i ~ b => While(c, i, b)
    }

  /* Parsing for Program. */
  def prog   : Parser[IMPProgram] =
    ("program" ~> pvar) ~ preblock ~ postblock ~ ("is" ~> block <~ "end") ^^ {
      case n ~ pre ~ post ~ b => (n, pre, post, b)
    }
}