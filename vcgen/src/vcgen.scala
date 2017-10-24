import scala.util.parsing.combinator._
import scala.language.postfixOps
import java.io.FileReader
import sys.process._

// our objects
import OurObjects._
import SMTGen._
import WeakestPreGen._
import GuardedGen._
import ImpParser._


object VCGen {

  def prettyPrint(wp: Assertion) = {

    def prettyArith(a: ArithExp) : String = a match {
      case Num(x) => x.toString
      case Var(v) => v
      case Read(n, i) => n.concat("[").concat(prettyArith(i)).concat("]")
      case Add(l, r) => prettyArith(l).concat(" + ").concat(prettyArith(r))
      case Sub(l, r) => prettyArith(l).concat(" - ").concat(prettyArith(r))
      case Mul(l, r) => prettyArith(l).concat(" * ").concat(prettyArith(r))
      case Div(l, r) => prettyArith(l).concat(" / ").concat(prettyArith(r))
      case Mod(l, r) => prettyArith(l).concat(" % ").concat(prettyArith(r))
      case Parens(a) => "(".concat(prettyArith(a)).concat(")")
    }

    def prettyBool(b: BoolExp) : String = b match {
      case False => "FALSE"
      case True => "TRUE"
      case BCmp(cmp) => prettyArith(cmp._1).concat(" ").concat(cmp._2).concat(" ").concat(prettyArith(cmp._3))
      case BNot(b) => "!(".concat(prettyBool(b)).concat(")")
      case BDisj(l, r) => prettyBool(l).concat(" || ").concat(prettyBool(r))
      case BConj(l, r) => prettyBool(l).concat(" && ").concat(prettyBool(r))
      case BParens(b) => "(".concat(prettyBool(b)).concat(")")
    }

    def prettyAssertion(wp: Assertion) : String = wp match {
      case Assn(b) => prettyBool(b)
      case ANot(a) => "!(".concat(prettyAssertion(a)).concat(")")
      case ADisj(l, r) => prettyAssertion(l).concat(" || ").concat(prettyAssertion(r))
      case AConj(l, r) => prettyAssertion(l).concat(" && ").concat(prettyAssertion(r))
      case AImp(l, r) => "(".concat(prettyAssertion(l)).concat(" ==> \n").concat(prettyAssertion(r)).concat(")")
      case ForAll(x, c) => "forall ".concat(x.mkString).concat(", ").concat(prettyAssertion(c))
      case Exists(x, c) => "exists ".concat(x.mkString).concat(", ").concat(prettyAssertion(c))
    }

    println(prettyAssertion(wp))
  }

  val testz3 = "(declare-fun x () Int)(declare-fun y () Int)(assert (>= x y))(check-sat)(get-model)(exit)"

  def main(args: Array[String]): Unit = {
    val reader = new FileReader(args(0))
    import ImpParser._;
    val imp = parseAll(prog, reader)
    // println("PARSE:")
    // println(imp)
    val guarded = makeGuarded(imp.get)
    // println("GUARDED:")
    // println(guarded)
    val wp = wpgen(guarded)
    // println("WEAKEST PRE:")
    // println(wp)

    // println("PRETTY:")
    // prettyPrint(wp)

    // test
    // val sat = ("echo " + testz3) #| "z3 -smt2 -in" !!;
    val sat = smtgen(wp)
    // println(sat)
    val out : String = ("echo " + sat) #| "z3 -smt2 -in" !!;
    // println("SAT:") 
    // println(out)
    if (out contains "unsat") {
      println("sat")
    } else {
      println("unsat")
    }
  }
}
