import scala.util.parsing.combinator._
import java.io.FileReader
import sys.process._

// our objects
import OurObjects._
import WeakestPreGen._
import GuardedGen._
import ImpParser._

object VCGen {

  val test = "(declare-fun x () Int)(declare-fun y () Int)(assert (>= x y))(check-sat)(get-model)(exit)"

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

    ("echo " + test) #| "z3 -smt2 -in" !
  }
}
