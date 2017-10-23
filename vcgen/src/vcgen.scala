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

  val testz3 = "(declare-fun x () Int)(declare-fun y () Int)(assert (>= x y))(check-sat)(get-model)(exit)"

  def main(args: Array[String]): Unit = {
    val reader = new FileReader(args(0))
    import ImpParser._;
    val imp = parseAll(prog, reader)
    println("PARSE:")
    println(imp)
    val guarded = makeGuarded(imp.get)
    println("GUARDED:")
    println(guarded)
    val wp = wpgen(guarded)
    println("WEAKEST PRE:")
    println(wp)
    // test
    // val sat = ("echo " + testz3) #| "z3 -smt2 -in" !!;
    val sat = smtgen(wp).mkString
    val out = ("echo " + sat) #| "z3 -smt2 -in" !!;
    println("SAT:") 
    println(out)
  }
}
