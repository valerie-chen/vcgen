import scala.util._
import Language._
import GuardedGen._

object WeakestPre {


	/* get all guarded vars -- using Anton's for now */
	def allGuardVars(full: GuardedProgram) : List[String] = {
		def aexpVars(aexp: ArithExp) : List[String] = aexp match {
			case Num(_) => Nil
			case Var(n) => List(n)
			case Read(n, i) => n :: aexpVars(i)
			case Add(l, r) => aexpVars(l) ::: aexpVars(r)
			case Sub(l, r) => aexpVars(l) ::: aexpVars(r)
			case Mul(l, r) => aexpVars(l) ::: aexpVars(r)
			case Div(l, r) => aexpVars(l) ::: aexpVars(r)
			case Mod(l, r) => aexpVars(l) ::: aexpVars(r)
			case Parens(a) => aexpVars(a)
		}

		def compVars(cmp: Comparison) : List[String] = {
			aexpVars(cmp._1) ::: aexpVars(cmp._3)
		}

		def bexpVars(bexp: BoolExp) : List[String] = bexp match {
			case True => Nil
			case False => Nil
			case BCmp(c) => compVars(c)
			case BNot(b) => bexpVars(b)
			case BDisj(l, r) => bexpVars(l) ::: bexpVars(r)
			case BConj(l, r) => bexpVars(r) ::: bexpVars(r)
			case BParens(b) => bexpVars(b)
		}

		def assnVars(assn: Assertion) : List[String] = assn match {
			case AssnBool(c) => bexpVars(c)
			case Negate(a) => assnVars(a)
			case Or(l, r) => assnVars(l) ::: assnVars(r)
			case And(l, r) => assnVars(l) ::: assnVars(r)
			case Implies(l, r) => assnVars(l) ::: assnVars(r)
			case ForAll(ns, a) => ns ::: assnVars(a)
			case Exists(ns, a) => ns ::: assnVars(a)
			case ParensAssn(a) => assnVars(a)
		}

		def guardVars(gcs: GuardedCmd) : List[String] = gcs match{
			case Assume(a) => assnVars(a)
			case Assert(a) => assnVars(a)
			case HavocVar(s) => List(s)
			case HavocArr(s, aexp) => s :: aexpVars(aexp)
			case Seq(gc1, gc2) => allGuardVars(gc1) ::: allGuardVars(gc2)
			case NonDet(gc1, gc2) => allGuardVars(gc1) ::: allGuardVars(gc2)
		}

		guardVars(full)
	}

	def wpComparison(old: String, ind: Option[ArithExp], tmp: String, cmp: Comparison) : Comparison = {
		return (aexpSub(old, ind, tmp, cmp._1), cmp._2, aexpSub(old, ind, tmp, cmp._3))
	}

	def wpBool(old: String, ind: Option[ArithExp], tmp: String, bool: BoolExp) = bool match {
		case True => True
		case False => False
		case BCmp(cmp) => BCmp(wpComparison(old, ind, tmp, cmp))
		case BNot(b) => BNot(wpBool(old, ind, tmp, b))
		case BDisj(l, r) => BDisj(wpBool(old, ind, tmp, l), wpBool(old, ind, tmp, r))
		case BConj(l, r) => BConj(wpBool(old, ind, tmp, l), wpBool(old, ind, tmp, r))
		case BParens(b) => BParens(wpBool(old, ind, tmp, l), wpBool(old, ind, tmp, r))
	}

	def wpAssert(old: String, ind: Option[ArithExp], tmp: String, assert: Assertion) : Assertion = assert match {
		case Assn(b) => Assn(wpBool(old, ind, tmp, b))
		case AParens(a) => AParens(wpAssert(old, ind, tmp, a)
		case ANot(a) => ANot(wpAssert(old, ind, tmp, a))
		case ADisj(l, r) => ADisj(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
		case AConj(l, r) => AConj(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
		case AImpl(l, r) => AImpl(wpAssert(old, ind, tmp, l), wpAssert(old, ind, tmp, r))
		case ForAll(x, c) => ForAll(x, if (x.contains(old)) {c} else {wpAssert(old, ind, tmp, c)})
		case Exists(x, c) => Exists(x, if (x.contains(old)) {c} else {wpAssert(old, ind, tmp, c)})
	}

	def wpgen(g: GuardedProgram) : Assertion = {
		var allvars = allGuardVars(gcs)

		def wp(gcs : GuardedProgram, a : Assertion) : Assertion = gcs match {
			case Assume(cmd) => AImpl(cmd, a)
			case Assert(cmd) => AConj(cmd, a)
			case HavocVar(x) => {
				val nxt = nextVar()
				allVars :: nxt
				wpAssert(x, None, nxt, a)
			}
			case HavocArr(x, i) => {
				val nxt = nextVar()
				allVars :: nxt
				wpAssert(x, Some(i), nxt, a)
			}
			case LogSplit(cmd1, cmd2) => AConj(wp(cmd1, a), wp(cmd2, a))
			case head :: tail => wp(head, wp(tail, a))
		}

		wp(g)
	}
}