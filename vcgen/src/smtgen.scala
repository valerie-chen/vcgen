import scala.util._
import OurObjects._
import WeakestPreGen._

object SMTGen {
	def smtgen(wp: Assertion) : String = {
		var z3 = ""
		var allvars = ""

		var wpvars = assnVars(wp).toSet
		while (!wpvars.IsEmpty) {
			var
		}
	}
}