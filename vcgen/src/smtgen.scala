import scala.util._
import OurObjects._
import WeakestPreGen._

object SMTGen {

	def allWpVars(wp: Assertion) : List[String] = {
		def arithVars(ar: ArithExp) : List[String] = ar match {
			case Num(_) => List()
			case Var(v) => List(v)
			case Read(n, i) => /*n :: */arithVars(i)
			case Add(l, r) => arithVars(l) ::: arithVars(r)
			case Sub(l, r) => arithVars(l) ::: arithVars(r)
			case Mul(l, r) => arithVars(l) ::: arithVars(r)
			case Div(l, r) => arithVars(l) ::: arithVars(r)
			case Mod(l, r) => arithVars(l) ::: arithVars(r)
			case Parens(a) => arithVars(a)	
		}
		def boolVars(b: BoolExp) : List[String] = b match {
			case False => List()
			case True => List()
			case BCmp(cmp) => arithVars(cmp._1) ::: arithVars(cmp._3)
			case BNot(b) => boolVars(b)
			case BDisj(left, right) => boolVars(left) ::: boolVars(right)
			case BConj(left, right) => boolVars(left) ::: boolVars(right)
			case BParens(b) => boolVars(b)
		}
		def assnVars(wp: Assertion) : List[String] = wp match {
			case Assn(b) => boolVars(b)
			case ANot(a) => assnVars(a)
			case ADisj(l, r) => assnVars(l) ::: assnVars(r)
			case AConj(l, r) => assnVars(l) ::: assnVars(r)
			case AImp(l, r) => assnVars(l) ::: assnVars(r)
			case ForAll(x, c) => x ::: assnVars(c)
			case Exists(x, c) => x ::: assnVars(c)
		}
		assnVars(wp)
	}

	def allWpArrs(wp: Assertion) : List[String] = {
		def arithVars(ar: ArithExp) : List[String] = ar match {
			case Num(_) => List()
			case Var(v) => List(/*v*/)
			case Read(n, i) => List(n) //n :: arithVars(i)
			case Add(l, r) => arithVars(l) ::: arithVars(r)
			case Sub(l, r) => arithVars(l) ::: arithVars(r)
			case Mul(l, r) => arithVars(l) ::: arithVars(r)
			case Div(l, r) => arithVars(l) ::: arithVars(r)
			case Mod(l, r) => arithVars(l) ::: arithVars(r)
			case Parens(a) => arithVars(a)	
		}
		def boolVars(b: BoolExp) : List[String] = b match {
			case False => List()
			case True => List()
			case BCmp(cmp) => arithVars(cmp._1) ::: arithVars(cmp._3)
			case BNot(b) => boolVars(b)
			case BDisj(left, right) => boolVars(left) ::: boolVars(right)
			case BConj(left, right) => boolVars(left) ::: boolVars(right)
			case BParens(b) => boolVars(b)
		}
		def assnVars(wp: Assertion) : List[String] = wp match {
			case Assn(b) => boolVars(b)
			case ANot(a) => assnVars(a)
			case ADisj(l, r) => assnVars(l) ::: assnVars(r)
			case AConj(l, r) => assnVars(l) ::: assnVars(r)
			case AImp(l, r) => assnVars(l) ::: assnVars(r)
			case ForAll(x, c) => x ::: assnVars(c)
			case Exists(x, c) => x ::: assnVars(c)
		}
		assnVars(wp)
	}

	def smtgen(wp: Assertion) : String = {
		def genArith(arr: ArithExp) : String = arr match {
			case Num(x) => x.toString
			case Var(v) => v
			case Read(n, i) => "(select ".concat(n).concat(" ").concat(genArith(i)).concat(")")
			case Add(l, r) => "(+ ".concat(genArith(l)).concat(" ").concat(genArith(r)).concat(")")
			case Sub(l, r) => "(- ".concat(genArith(l)).concat(" ").concat(genArith(r)).concat(")")
			case Mul(l, r) => "(* ".concat(genArith(l)).concat(" ").concat(genArith(r)).concat(")")
			case Div(l, r) => "(/ ".concat(genArith(l)).concat(" ").concat(genArith(r)).concat(")")
			case Mod(l, r) => "(mod ".concat(genArith(l)).concat(" ").concat(genArith(r)).concat(")")
			case Parens(a) => genArith(a)
		}
		def genBool(b: BoolExp) : String = b match {
			case False => "false"
			case True => "true"
			case BCmp(cmp) => cmp._2 match {
				case "!=" => "(not (= ".concat(genArith(cmp._1)).concat(" ").concat(genArith(cmp._3)).concat("))")
				case x => "(".concat(x).concat(" ").concat(genArith(cmp._1)).concat(" ").concat(genArith(cmp._3)).concat(")")
			}
			case BNot(b) => "(not ".concat(genBool(b)).concat(")")
			case BDisj(left, right) => "(or ".concat(genBool(left)).concat(" ").concat(genBool(right)).concat(")")
			case BConj(left, right) => "(and ".concat(genBool(left)).concat(" ").concat(genBool(right)).concat(")")
			case BParens(b) => genBool(b)
		}
		def varFormat(vars: List[String]) : List[String] = {
			vars.map( n => "(".concat(n).concat(" Int)"))
		}
		def genAssn(assn: Assertion) : String = assn match {
			case Assn(b) => genBool(b)
			case ANot(a) => "(not ".concat(genAssn(a)).concat(")")
			case ADisj(a, b) => "(or ".concat(genAssn(a)).concat(" ").concat(genAssn(b)).concat(")")
			case AConj(a, b) => "(and ".concat(genAssn(a)).concat(" ").concat(genAssn(b)).concat(")")
			case AImp(a, b) => "(=> ".concat(genAssn(a)).concat(" ").concat(genAssn(b)).concat(")")
			case ForAll(x, cond) => {
				val formattedX = varFormat(x)
				"(forall (".concat(formattedX.mkString(" ")).concat(") ").concat(genAssn(cond)).concat(")")
			}
			case Exists(x, cond) => {
				val formattedX = varFormat(x)
				"(exists (".concat(formattedX.mkString(" ")).concat(") ").concat(genAssn(cond)).concat(")")
			}
			//"(exists (".concat(x.mkString(" ")).concat(") ").concat(genAssn(cond)).concat(")")
		}

		var smt = ""
		val wpArrs = allWpArrs(wp).toSet
	  val wpVars = allWpVars(wp).toSet--wpArrs
	  wpVars.foreach { v => 
	  	smt = smt.concat("(declare-fun ".concat(v).concat(" () Int)\n"))
		}
		wpArrs.foreach(v => (smt = smt.concat("(declare-const ".concat(v).concat(" (Array Int Int))\n"))))

    smt = smt.concat("(assert (not ").concat(genAssn(wp)).concat("))\n")
   	smt.concat("(check-sat)\n") // (get-model)
	}
}