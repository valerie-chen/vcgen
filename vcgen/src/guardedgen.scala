import scala.util._
import OurObjects._

object GuardedGen {

	/* Parsing the list of statements. */
	def mGuard(block: Block) : GuardedProgram = block match {
		case Nil => ()
		case s :: right => s match {
			case Assign(x, value) => {
				var tmp = nextVar()
				var a1 = Assume(Assn(BCmp((Var(tmp), "=", Var(x)))))
				var a2 = HavocVar(x)
				var a3 = Assume(Assn(BCmp((Var(x), "=", ??))))
				a1 :: a2 :: a3 :: mGuard(right)
			}
			case Write(x, i, value) => {
				// GOTTA MODIFY LOTS
				var tmp = nextVar()
				var a1 = Assume(Assn(BCmp((Var(tmp), "=", Var(x)))))
				var a2 = HavocVar(x)
				var a3 = Assume(Assn(BCmp((Var(x), "=", ??))))
				a1 :: a2 :: a3 :: mGuard(right)
			}
			case ParAssign(x1, x2, value1, value2) => {
				mGuard(List(Assign(x1, value1), Assign(x2, value2))) ::: mGuard(right)
			}
			case If(cond, th, el) => {
				var a1 = List(Assume(Assn(cond)), mGuard(th))
				var a2 = List(Assume(Assn(BNot(cond))), mGuard(el))
				LogSplit(a1, a2) :: mGuard(right)
			}
			case While(cond, inv, body) => {
				var a1 = Assert(Assn(inv))
				var havocs = () // all variables in body
				var a2 = Assume(Assn(inv))
				var a3 = List(Assume(Assn(cond)), mGuard(body), Assert(Assn(inv)), Assume(Assn(False)))
				var a4 = Assume(Assn(BNot(cond)))
				(a1 :: havocs) ::: a2 ::: LogSplit(a3, a4) ::: mGuard(right)
			}
		}
	}

  /* Parsing for Program. */
  def makeGuarded(prog: IMPProgram) : GuardedProgram = {
  	var pre = prog._2.map(Assume(_))
  	var post = prog._3.map(Assert(_))
  	pre ::: mGuard(prog._4) ::: post
  }
}