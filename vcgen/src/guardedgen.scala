import scala.util._
import OurObjects._

object GuardedGen {
	var dummyVarIndex = 0
	var impVars : Set[String] = Set()

	def nextVar(v: String) : String = {
		var newVar : String = v.concat("1")
		while (impVars.contains(newVar)) {
			newVar = newVar.concat("1")
		}
		impVars = impVars + newVar
		newVar
		// val dummyString = "dummyString"
		// var newDummyVar = dummyString.concat(dummyVarIndex.toString)
		// dummyVarIndex += 1
		// newDummyVar
	}

	def havocsFromModifiedVars(block: Block) : GuardedProgram = block match {
		case Nil => List()
		case s :: right => s match {
			case Assign(x, v) => HavocVar(x) :: havocsFromModifiedVars(right)
			case Write(x, i, v) => HavocArray(x, i) :: havocsFromModifiedVars(right)
			case ParAssign(x1, x2, v1, v2) => HavocVar(x1) :: (HavocVar(x2) :: havocsFromModifiedVars(right))
			case If(cond, th, el) => havocsFromModifiedVars(th) ::: havocsFromModifiedVars(el) ::: havocsFromModifiedVars(right)
			case While(cond, inv, body) => havocsFromModifiedVars(body) ::: havocsFromModifiedVars(right)
		}
	}

	def allVars(prog: IMPProgram) : List[String] = {

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

		def assertionVars(block: AssertionBlock) : List[String] = block match {
			case Nil => List()
			case assn :: right => assn match {
				case Assn(b) => boolVars(b) ::: assertionVars(right)
				case ANot(a) => assertionVars(a :: right)
				case ADisj(a, b) => assertionVars(a :: (b :: right))
				case AConj(a, b) => assertionVars(a :: (b :: right))
				case AImp(a, b) => assertionVars(a :: (b :: right))
				case ForAll(x, cond) => x ::: assertionVars(List(cond)) ::: assertionVars(right)
				case Exists(x, cond) => x ::: assertionVars(List(cond)) ::: assertionVars(right)
			}
		}

		def blockVars(block: Block) : List[String] = block match {
			case Nil => List()
			case s :: right => s match {
				case Assign(x, value) => x :: arithVars(value) ::: blockVars(right)
				case Write(x, ind, value) => x :: arithVars(ind) ::: arithVars(value) ::: blockVars(right)
				case ParAssign(x1, x2, value1, value2) => x1 :: (x2 :: arithVars(value1) ::: arithVars(value2)) ::: blockVars(right)
				case If(cond, th, el) => boolVars(cond) ::: blockVars(th) ::: blockVars(el) ::: blockVars(right)
				case While(cond, inv, body) => boolVars(cond) ::: assertionVars(inv) ::: blockVars(body) ::: blockVars(right)
			}
		}

		(prog._1 :: assertionVars(prog._2) ::: assertionVars(prog._3) ::: blockVars(prog._4)).distinct
	}

	def substituteVar(name: String, index: Option[ArithExp], value: ArithExp, sub: String) : ArithExp = value match {
		case Num(v) => Num(v)
		case Var(n) => index match {
			case None => {
				if (n == name) { 
					Var(sub)
				} else {
					Var(name)
				}
			}
			case Some(i) => Var(name)
		}
		case Read(n, i) => index match {
			case None => Read(n, i)
			case Some(i2) => {
				if (i == i2 && n == name) {
					Read(sub, i)// Var(sub)
				} else {
					Read(n, i)
				}
			}
		}
		case Add(left, right) => Add(substituteVar(name, index, left, sub), substituteVar(name, index, right, sub))
		case Sub(left, right) => Sub(substituteVar(name, index, left, sub), substituteVar(name, index, right, sub))
		case Mul(left, right) => Mul(substituteVar(name, index, left, sub), substituteVar(name, index, right, sub))
		case Div(left, right) => Div(substituteVar(name, index, left, sub), substituteVar(name, index, right, sub))
		case Mod(left, right) => Mod(substituteVar(name, index, left, sub), substituteVar(name, index, right, sub))
		case Parens(a) => Parens(substituteVar(name, index, a, sub))
	}

  /* Parsing for Program. */
  def makeGuarded(prog: IMPProgram) : GuardedProgram = {
  	impVars = allVars(prog).toSet

	  /* Parsing the list of statements. */
		def mGuard(block: Block) : GuardedProgram = block match {
			case Nil => List()
			case s :: right => s match {
				case Assign(x, value) => {
					var tmp = nextVar(x)
					var a1 = Assume(Assn(BCmp((Var(tmp), "=", Var(x)))))
					var a2 = HavocVar(x)
					var a3 = Assume(Assn(BCmp((Var(x), "=", substituteVar(x, None, value, tmp)))))
					a1 :: a2 :: a3 :: mGuard(right)
				}
				case Write(x, i, value) => {
					// GOTTA MODIFY LOTS
					var tmp = nextVar(x)
					var a1 = Assume(Assn(BCmp((Read(tmp, i), "=", Read(x, i)))))
					var a2 = HavocArray(x, i)
					var a3 = Assume(Assn(BCmp((Read(x, i), "=", substituteVar(x, Some(i), value, tmp)))))
					a1 :: a2 :: a3 :: mGuard(right)
				}
				case ParAssign(x1, x2, value1, value2) => {
					mGuard(List(Assign(x1, value1), Assign(x2, value2))) ::: mGuard(right)
				}
				case If(cond, th, el) => {
					var a1 = Assume(Assn(cond)) :: mGuard(th)
					var a2 = Assume(Assn(BNot(cond))) :: mGuard(el)
					LogSplit(a1, a2) :: mGuard(right)
				}
				case While(cond, inv, body) => {
					var a1 = inv.map(Assert(_))
					var havocs = havocsFromModifiedVars(body)
					var a2 = inv.map(Assume(_))
					var a3 = Assume(Assn(cond)) :: mGuard(body) ::: inv.map(Assert(_)) ::: List(Assume(Assn(False)))
					var a4 = List(Assume(Assn(BNot(cond))))
					a1 ::: havocs ::: a2 ::: List(LogSplit(a3, a4)) ::: mGuard(right)
				}
			}
		}

  	var pre = prog._2.map(Assume(_))
  	var post = prog._3.map(Assert(_))
  	pre ::: mGuard(prog._4) ::: post
  }
}