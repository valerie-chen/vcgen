import scala.util.parsing.combinator._
import java.io.FileReader

// our objects
import OurObjects._
import GuardedGen._
import ImpParser._

object VCGen {

  def main(args: Array[String]): Unit = {
    val reader = new FileReader(args(0))
    import ImpParser._;
    val imp = parseAll(prog, reader)
    println("PARSE:")
    println(imp)
    val guarded = makeGuarded(imp.get)
    println("GUARDED:")
    println(guarded)
  }
}
