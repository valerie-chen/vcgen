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

	def modify(assn: Assertion, name: String, index: Option[ArithExp], sub: String) : Assertion = {

		def modifyArith(ar: ArithExp, name: String, index: Option[ArithExp], sub: String) : ArithExp = ar match {
			case Num(x) => Num(x)
			case Var(v) => if (v == name) { Var(sub) } else { Var(v) }
			case Read(n, i) => index match {
				case None => Read(n, i)
				case Some(x) => if (n == name && i == x) { Read(sub, i) } else { Read(n, i) }
			}
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

		modifyAssn(assn, name, index, sub)

		// def modifyGuarded(g: GuardedProgram, name: String, index: Option[ArithExp], sub: String) : GuardedProgram = g match {
		// 	case Nil => List()
		// 	case c :: right => c match {
		// 		case Assume(cmd) => Assume(modifyAssn(cmd, name, index, sub)) :: modifyGuarded(right, name, index, sub)
		// 		case Assert(cmd) => Assert(modifyAssn(cmd, name, index, sub)) :: modifyGuarded(right, name, index, sub)
		// 		case HavocVar(x) => {
		// 			if (x == name) {
		// 				HavocVar(sub) :: modifyGuarded(right, name, index, sub)
		// 			} else {
		// 				HavocVar(x) :: modifyGuarded(right, name, index, sub)
 	// 				}
		// 		}
		// 		case HavocArray(x, i) => {
		// 			if (x == name && i == index.get) {
		// 				HavocArray(sub, i) :: modifyGuarded(right, name, index, sub) // ???????
		// 			} else {
		// 				HavocArray(x, i) :: modifyGuarded(right, name, index, sub) // ????? 
		// 			}
		// 		}
		// 		case LogSplit(cmd1, cmd2) => 
		// 			LogSplit(modifyGuarded(cmd1, name, index, sub), modifyGuarded(cmd2, name, index, sub)) :: modifyGuarded(right, name, index, sub)
		// 	}
		// }

		// modifyGuarded(g, name, index, sub)
	}

	def reverseWP(g: GuardedProgram) : GuardedProgram = {
		def reverseGP(g: GuardedProgram) : GuardedProgram = g match {
			case Nil => List()
			case s :: right => s match {
				case Assume(cmd) => reverseGP(right) ::: List(Assume(cmd))
				case Assert(cmd) => reverseGP(right) ::: List(Assert(cmd))
				case HavocVar(x) => reverseGP(right) ::: List(HavocVar(x))
				case HavocArray(x, i) => reverseGP(right) ::: List(HavocArray(x, i))
				case LogSplit(cmd1, cmd2) => reverseGP(right) ::: List(LogSplit(reverseGP(cmd2), reverseGP(cmd1)))
			}
		}
		reverseGP(g)
	}

	def wpgen(g: GuardedProgram) : Assertion = {
		def wp(gp: GuardedProgram, a : Assertion) : Assertion = gp match {
			case Nil => a
			case s :: right => s match {
				case Assume(cmd) => wp(right, AImp(cmd, a))
				case Assert(cmd) => wp(right, AConj(cmd, a))
				case HavocVar(x) => {
					val nxt = nextVar(x)
					// wp(modifyRight(right, x, None, nxt), a) //
					wp(right, modify(a, x, None, nxt))
				}
				case HavocArray(x, i) => {
					val nxt = nextVar(x)
					wp(right, modify(a, x, Some(i), nxt))
					// wp(modifyRight(right, x, Some(i), nxt), a)
				}
				case LogSplit(cmd1, cmd2) => wp(right, AConj(wp(cmd1, a), wp(cmd2, a)))
			}
		}

		wp(reverseWP(g), Assn(True))
	}
}