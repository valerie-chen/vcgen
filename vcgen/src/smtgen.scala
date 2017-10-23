import scala.util._
import OurObjects._
import WeakestPreGen._

object SMTGen {

	// get vars in aexp

	// get vars in comparison

	// get vars in bexp

	// get vars in assertions

// ------

	// get arrs in aexp

	// get arrs in comparison

	// get arrs in bexp

	// get arrs in assertions
	
// -----

	//parse aexp

	//parse comparison

	//parse bexp

	//parse assertions


	def smtgen(wp: Assertion) : String = {
		var z3 = ""
		var allVars = ""
    
	    var wpVars = assnVars(wp).toSet
	    while (!wpVars.isEmpty) {
	    	z3 = z3 + "(declare-fun " + wpVars.head + " () Int)\n"
	    	allVars = allVars + "(" + wpVars.head + " Int)"
	    	wpVars = wpVars.tail
	    }
	    var wpArrs = assnArrs(wp).toSet
	    while (!wpArrs.isEmpty) {
	    	z3 = z3 + "(declare-const " + wpArrs.head + " (Array Int Int))\n"
	    	wpArrs = wpArrs.tail
	    }
	    
	    return z3 + "(assert (forall (" + forVars + ") " + assertSt(weakest) + "))\n"  + "(check-sat)\n"
	}
}