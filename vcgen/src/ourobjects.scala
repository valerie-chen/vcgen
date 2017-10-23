import scala.util.parsing.combinator._
import java.io.FileReader

object OurObjects {

  /* Arithmetic expressions. */
  trait ArithExp

  case class Num(value: Int) extends ArithExp
  case class Var(name: String) extends ArithExp
  case class Read(name: String, ind: ArithExp) extends ArithExp
  case class Add(left: ArithExp, right: ArithExp) extends ArithExp
  case class Sub(left: ArithExp, right: ArithExp) extends ArithExp
  case class Mul(left: ArithExp, right: ArithExp) extends ArithExp
  case class Div(left: ArithExp, right: ArithExp) extends ArithExp
  case class Mod(left: ArithExp, right: ArithExp) extends ArithExp
  case class Parens(a: ArithExp) extends ArithExp


  /* Comparisons of arithmetic expressions. */
  type Comparison = Product3[ArithExp, String, ArithExp]


  /* Boolean expressions. */
  trait BoolExp

  case object False extends BoolExp
  case object True extends BoolExp
  case class BCmp(cmp: Comparison) extends BoolExp
  case class BNot(b: BoolExp) extends BoolExp
  case class BDisj(left: BoolExp, right: BoolExp) extends BoolExp
  case class BConj(left: BoolExp, right: BoolExp) extends BoolExp
  case class BParens(b: BoolExp) extends BoolExp
  
  /* Assertions and assert blocks. */
  trait Assertion
  type AssertionBlock = List[Assertion]

  case class Assn(b: BoolExp) extends Assertion
  case class ANot(b: Assertion) extends Assertion
  case class ADisj(left: Assertion, right: Assertion) extends Assertion
  case class AConj(left: Assertion, right: Assertion) extends Assertion
  case class AImp(left: Assertion, right: Assertion) extends Assertion
  case class ForAll(x: String, cond: Assertion) extends Assertion
  case class Exists(x: String, cond: Assertion) extends Assertion

  // trait Annotation
  // type AnnotationBlock = List[Annotation]

  // // class Post(a: AssertionBlock)
  // // class Pre(a: AssertionBlock)
  // // class Inv(a: AssertionBlock)
  // case class Post(a: Assertion) extends Annotation
  // case class Pre(a: Assertion) extends Annotation
  // case class Inv(a: Assertion) extends Annotation

  /* Statements and blocks. */
  trait Statement
  type Block = List[Statement]

  case class Assign(x: String, value: ArithExp) extends Statement
  case class Write(x: String, ind: ArithExp, value: ArithExp) extends Statement
  case class ParAssign(x1: String, x2: String, value1: ArithExp, value2: ArithExp) extends Statement
  case class If(cond: BoolExp, th: Block, el: Block) extends Statement
  case class While(cond: BoolExp, inv: AssertionBlock, body: Block) extends Statement

  /* Complete programs. */
  type IMPProgram = Product4[String, AssertionBlock, AssertionBlock, Block]

  trait GComm
  type GuardedProgram = List[GComm]

  case class Assume(a: Assertion) extends GComm
  case class Assert(a: Assertion) extends GComm
  case class HavocVar(x: String) extends GComm
  case class HavocArray(x: String, i: ArithExp) extends GComm
  case class LogSplit(a: GuardedProgram, b: GuardedProgram) extends GComm
}
