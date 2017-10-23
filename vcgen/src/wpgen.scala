import scala.util._
import OurObjects._
import GuardedGen._

object WeakestPreGen {
	var dummyVarIndex = 0

	def nextVar() : String = {
		val dummyString = "dummyString"
		var newDummyVar = dummyString.concat(dummyVarIndex.toString)
		dummyVarIndex += 1
		newDummyVar
	}

	def allGuardVars(prog: GuardedProgram) : List[String] = {

		def arithVars(a: ArithExp) : List[String] = a match {
			case Num(_) => Nil
			case Var(v) => List(v)
			case Read(n, i) => n :: arithVars(i)
			case Add(l, r) => arithVars(l) ::: arithVars(r)
			case Sub(l, r) => arithVars(l) ::: arithVars(r)
			case Mul(l, r) => arithVars(l) ::: arithVars(r)
			case Div(l, r) => arithVars(l) ::: arithVars(r)
			case Mod(l, r) => arithVars(l) ::: arithVars(r)
			case Parens(a) => arithVars(a)
		}

		def boolVars(b: BoolExp) : List[String] = b match {
			case False => Nil
			case True => Nil
			case BCmp(cmp) => arithVars(cmp._1) ::: arithVars(cmp._3)
			case BNot(b) => boolVars(b)
			case BDisj(left, right) => boolVars(left) ::: boolVars(right)
			case BConj(left, right) => boolVars(left) ::: boolVars(right)
			case BParens(b) => boolVars(b)
		}

		def assertionVars(a: Assertion) : List[String] = a match {
			case Assn(b) => boolVars(b)
			case ANot(a) => assertionVars(a)
			case ADisj(a, b) => assertionVars(a) ::: assertionVars(b)
			case AConj(a, b) => assertionVars(a) ::: assertionVars(b)
			case AImp(a, b) => assertionVars(a) ::: assertionVars(b)
			case ForAll(x, cond) => x ::: assertionVars(cond)
			case Exists(x, cond) => x ::: assertionVars(cond)
		}

		def progGuardVars(prog: GuardedProgram) : List[String] = prog match {
			case Nil => List()
			case c :: right => c match {
				case Assume(a) => assertionVars(a) ::: allGuardVars(right)
				case Assert(a) => assertionVars(a) ::: allGuardVars(right)
				case HavocVar(x) => x :: allGuardVars(right)
				case HavocArray(x, i) => x :: arithVars(i) ::: allGuardVars(right)
				case LogSplit(a, b) => allGuardVars(a) ::: allGuardVars(b) ::: allGuardVars(right)
			}
		}	
		
		progGuardVars(prog).distinct
	}	

	def wpComparison(old: String, ind: Option[ArithExp], tmp: String, cmp: Comparison) : Comparison = {
		return (substituteVar(old, ind, cmp._1, tmp), cmp._2, substituteVar(old, ind, cmp._3, tmp))
	}

	def wpBool(old: String, ind: Option[ArithExp], tmp: String, bool: BoolExp) : BoolExp = bool match {
		case True => True
		case False => False
		case BCmp(cmp) => BCmp(wpComparison(old, ind, tmp, cmp))
		case BNot(b) => BNot(wpBool(old, ind, tmp, b))
		case BDisj(l, r) => BDisj(wpBool(old, ind, tmp, l), wpBool(old, ind, tmp, r))
		case BConj(l, r) => BConj(wpBool(old, ind, tmp, l), wpBool(old, ind, tmp, r))
		case BParens(b) => BParens(wpBool(old, ind, tmp, b))
	}

	def wpAssert(old: String, ind: Option[ArithExp], tmp: String, assert: Assertion) : Assertion = assert match {
		case Assn(b) => Assn(wpBool(old, ind, tmp, b))
		//case AParens(a) => AParens(wpAssert(old, ind, tmp, a)
		case ANot(a) => ANot(wpAssert(old, ind, tmp, a))
		case ADisj(l, r) => ADisj(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
		case AConj(l, r) => AConj(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
		case AImp(l, r) => AImp(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
		case ForAll(x, c) => ForAll(x, if (x.contains(old)) {c} else {wpAssert(old, ind, tmp, c)})
		case Exists(x, c) => Exists(x, if (x.contains(old)) {c} else {wpAssert(old, ind, tmp, c)})
	}

	def wpgen(g: GuardedProgram) : Assertion = {
		var allvars = allGuardVars(g)

		def wp(gp : GuardedProgram, a : Assertion) : Assertion = gp match {
			case Nil => Assn(True)
			case s :: right => s match {
				case Assume(cmd) => AImp(cmd, a)
				case Assert(cmd) => AConj(cmd, a)
				case HavocVar(x) => {
					val nxt = nextVar()
					//allVars :: nxt
					wpAssert(x, None, nxt, a)
				}
				// case HavocArr(x, i) => {
				// 	val nxt = nextVar()
				// 	//allVars :: nxt
				// 	wpAssert(x, Some(i), nxt, a)
				// }
				case LogSplit(cmd1, cmd2) => AConj(wp(cmd1, a), wp(cmd2, a))
				//case head :: tail => wp(head, wp(tail, a))
			}
		}

		wp(g, Assn(True))
	}
}