import scala.util._
import OurObjects._
import GuardedGen._

object WeakestPreGen {
	var dummyVarIndex = 0

	def nextVar(v: String) : String = {
		// val dummyString = "dummyGuard"
		// var newDummyVar = dummyString.concat(dummyVarIndex.toString)
		// dummyVarIndex += 1
		// newDummyVar
		val dummy : String =	v.concat("a1")
		dummy
	}

	// def allGuardVars(prog: GuardedProgram) : List[String] = {

	// 	def arithVars(a: ArithExp) : List[String] = a match {
	// 		case Num(_) => Nil
	// 		case Var(v) => List(v)
	// 		case Read(n, i) => n :: arithVars(i)
	// 		case Add(l, r) => arithVars(l) ::: arithVars(r)
	// 		case Sub(l, r) => arithVars(l) ::: arithVars(r)
	// 		case Mul(l, r) => arithVars(l) ::: arithVars(r)
	// 		case Div(l, r) => arithVars(l) ::: arithVars(r)
	// 		case Mod(l, r) => arithVars(l) ::: arithVars(r)
	// 		case Parens(a) => arithVars(a)
	// 	}

	// 	def boolVars(b: BoolExp) : List[String] = b match {
	// 		case False => Nil
	// 		case True => Nil
	// 		case BCmp(cmp) => arithVars(cmp._1) ::: arithVars(cmp._3)
	// 		case BNot(b) => boolVars(b)
	// 		case BDisj(left, right) => boolVars(left) ::: boolVars(right)
	// 		case BConj(left, right) => boolVars(left) ::: boolVars(right)
	// 		case BParens(b) => boolVars(b)
	// 	}

	// 	def assertionVars(a: Assertion) : List[String] = a match {
	// 		case Assn(b) => boolVars(b)
	// 		case ANot(a) => assertionVars(a)
	// 		case ADisj(a, b) => assertionVars(a) ::: assertionVars(b)
	// 		case AConj(a, b) => assertionVars(a) ::: assertionVars(b)
	// 		case AImp(a, b) => assertionVars(a) ::: assertionVars(b)
	// 		case ForAll(x, cond) => x ::: assertionVars(cond)
	// 		case Exists(x, cond) => x ::: assertionVars(cond)
	// 	}

	// 	def progGuardVars(prog: GuardedProgram) : List[String] = prog match {
	// 		case Nil => List()
	// 		case c :: right => c match {
	// 			case Assume(a) => assertionVars(a) ::: allGuardVars(right)
	// 			case Assert(a) => assertionVars(a) ::: allGuardVars(right)
	// 			case HavocVar(x) => x :: allGuardVars(right)
	// 			case HavocArray(x, i) => x :: arithVars(i) ::: allGuardVars(right)
	// 			case LogSplit(a, b) => allGuardVars(a) ::: allGuardVars(b) ::: allGuardVars(right)
	// 		}
	// 	}	
		
	// 	progGuardVars(prog).distinct
	// }	

	// def wpComparison(old: String, ind: Option[ArithExp], tmp: String, cmp: Comparison) : Comparison = {
	// 	return (substituteVar(old, ind, cmp._1, tmp), cmp._2, substituteVar(old, ind, cmp._3, tmp))
	// }

	// def wpBool(old: String, ind: Option[ArithExp], tmp: String, bool: BoolExp) : BoolExp = bool match {
	// 	case True => True
	// 	case False => False
	// 	case BCmp(cmp) => BCmp(wpComparison(old, ind, tmp, cmp))
	// 	case BNot(b) => BNot(wpBool(old, ind, tmp, b))
	// 	case BDisj(l, r) => BDisj(wpBool(old, ind, tmp, l), wpBool(old, ind, tmp, r))
	// 	case BConj(l, r) => BConj(wpBool(old, ind, tmp, l), wpBool(old, ind, tmp, r))
	// 	case BParens(b) => BParens(wpBool(old, ind, tmp, b))
	// }

	// def wpAssert(old: String, ind: Option[ArithExp], tmp: String, assert: Assertion) : Assertion = assert match {
	// 	case Assn(b) => Assn(wpBool(old, ind, tmp, b))
	// 	//case AParens(a) => AParens(wpAssert(old, ind, tmp, a)
	// 	case ANot(a) => ANot(wpAssert(old, ind, tmp, a))
	// 	case ADisj(l, r) => ADisj(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
	// 	case AConj(l, r) => AConj(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
	// 	case AImp(l, r) => AImp(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
	// 	case ForAll(x, c) => ForAll(x, if (x.contains(old)) {c} else {wpAssert(old, ind, tmp, c)})
	// 	case Exists(x, c) => Exists(x, if (x.contains(old)) {c} else {wpAssert(old, ind, tmp, c)})
	// }

	def modifyRight(g: GuardedProgram, name: String, index: Option[ArithExp], sub: String) : GuardedProgram = {

		def modifyArith(ar: ArithExp, name: String, index: Option[ArithExp], sub: String) : ArithExp = ar match {
			case Num(x) => Num(x)
			case Var(v) => if (v == name) { Var(sub) } else { Var(v) }
			case Read(n, i) => if (n == name && i == index.get) { Read(sub, i) } else { Read(n, i) }
			case Add(l, r) => Add(modifyArith(l, name, index, sub), modifyArith(r, name, index, sub))
			case Sub(l, r) => Sub(modifyArith(l, name, index, sub), modifyArith(r, name, index, sub))
			case Mul(l, r) => Mul(modifyArith(l, name, index, sub), modifyArith(r, name, index, sub))
			case Div(l, r) => Div(modifyArith(l, name, index, sub), modifyArith(r, name, index, sub))
			case Mod(l, r) => Mod(modifyArith(l, name, index, sub), modifyArith(r, name, index, sub))
			case Parens(a) => Parens(modifyArith(a, name, index, sub))		
		}

		def modifyBool(bool: BoolExp, name: String, index: Option[ArithExp], sub: String) : BoolExp = bool match {
			case False => False
			case True => True
			case BCmp(cmp) => BCmp((modifyArith(cmp._1, name, index, sub), cmp._2, modifyArith(cmp._3, name, index, sub)))
			case BNot(b) => BNot(modifyBool(b, name, index, sub))
			case BDisj(left, right) => BDisj(modifyBool(left, name, index, sub), modifyBool(right, name, index, sub))
			case BConj(left, right) => BConj(modifyBool(left, name, index, sub), modifyBool(right, name, index, sub))
			case BParens(b) => BParens(modifyBool(b, name, index, sub))
		}

		def modifyAssn(assn: Assertion, name: String, index: Option[ArithExp], sub: String) : Assertion = assn match {
			case Assn(b) => Assn(modifyBool(b, name, index, sub))
			case ANot(a) => ANot(modifyAssn(a, name, index, sub))
			case ADisj(l, r) => ADisj(modifyAssn(l, name, index, sub), modifyAssn(r, name, index, sub))
			case AConj(l, r) => AConj(modifyAssn(l, name, index, sub), modifyAssn(r, name, index, sub))
			case AImp(l, r) => AImp(modifyAssn(l, name, index, sub), modifyAssn(r, name, index, sub))
			case ForAll(x, c) => {
				ForAll(x.map( n => if (n == name) sub else n ), modifyAssn(c, name, index, sub))
			}
			case Exists(x, c) => {
				Exists(x.map( n => if (n == name) sub else n ), modifyAssn(c, name, index, sub))
			}
		}

		def modifyGuarded(g: GuardedProgram, name: String, index: Option[ArithExp], sub: String) : GuardedProgram = g match {
			case Nil => List()
			case c :: right => c match {
				case Assume(cmd) => Assume(modifyAssn(cmd, name, index, sub)) :: modifyGuarded(right, name, index, sub)
				case Assert(cmd) => Assert(modifyAssn(cmd, name, index, sub)) :: modifyGuarded(right, name, index, sub)
				case HavocVar(x) => {
					if (x == name) {
						HavocVar(sub) :: modifyGuarded(right, name, index, sub)
					} else {
						HavocVar(x) :: modifyGuarded(right, name, index, sub)
 					}
				}
				case HavocArray(x, i) => {
					if (x == name && i == index.get) {
						HavocArray(sub, i) :: modifyGuarded(right, name, index, sub) // ???????
					} else {
						HavocArray(x, i) :: modifyGuarded(right, name, index, sub) // ????? 
					}
				}
				case LogSplit(cmd1, cmd2) => 
					LogSplit(modifyGuarded(cmd1, name, index, sub), modifyGuarded(cmd2, name, index, sub)) :: modifyGuarded(right, name, index, sub)
			}
		}

		modifyGuarded(g, name, index, sub)
	}

	def wpgen(g: GuardedProgram) : Assertion = {
		def wp(gp: GuardedProgram, a : Assertion) : Assertion = gp match {
			case Nil => a
			case s :: right => s match {
				case Assume(cmd) => wp(right, AImp(cmd, a))
				case Assert(cmd) => wp(right, AConj(cmd, a))
				case HavocVar(x) => {
					val nxt = nextVar(x)
					wp(modifyRight(right, x, None, nxt), a) //
				}
				case HavocArray(x, i) => {
					val nxt = nextVar(x)
					wp(modifyRight(right, x, Some(i), nxt), a)
				}
				case LogSplit(cmd1, cmd2) => wp(right, AConj(wp(cmd1, a), wp(cmd2, a)))
			}
		}

		// var allvars = allGuardVars(g)

		// def wp(gp : GuardedProgram/*, a : Assertion, gvars : List[String]*/) : Assertion = gp match {
		// 	case Nil => Assn(True)
		// 	case s :: right => s match {
		// 		case Assume(cmd) => AImp(cmd, wp(right))
		// 		case Assert(cmd) => AConj(cmd, wp(right))
		// 		case HavocVar(x) => {
		// 			val nxt = nextVar()
		// 			wpAssert(x, None, nxt, wp(right/*, a, nxt :: gvars*/))
		// 		}
		// 		case HavocArray(x, i) => {
		// 			val nxt = nextVar()
		// 			wpAssert(x, Some(i), nxt, wp(right/*, a, nxt :: gvars*/))
		// 		}
		// 		case LogSplit(cmd1, cmd2) => AConj(wp(cmd1), wp(cmd2))
		// 	}
		// }

		wp(g.reverse, Assn(True))
		// wp(g, Assn(True), allvars)
	}
}