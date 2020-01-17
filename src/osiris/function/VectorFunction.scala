// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._
import osiris.function.bilinear._
import osiris.function.linear.{Addition, LinearFunction, ScalarProduct, Sum}
import osiris.function.linear.reindex.Extract
import vector._
import morphism._
import osiris.function.map.{BiMap, ElemWise}
import osiris.vector.space.{MatrixSpace, SingleSpace, VectorSpace}

import scala.reflect.ClassTag

trait VectorFunction[I,J,S] extends (Vector[J,S] => Vector[I,S]) {

  /* ---------------------------------------------------------------------------------------------------------------- */

  val domain:VectorSpace[J,S]
  val target:VectorSpace[I,S]

  protected def scalarSpace = utilities.same(domain.scalarSpace,target.scalarSpace)

  def serialize:Iterable[Byte]

  def apply(x:pin.Pin[J,S]):pin.Pin[I,S] = {
    val comp = new pin.node.FunctionNode(this)
    x ->- comp.in
    comp.out
  }

  /* ---------------------------------------------------------------------------------------------------------------- */

  def <<[J0](that:VectorFunction[J,J0,S]):VectorFunction[I,J0,S] = Composition(this,that)

  def |[I2,J2](that:VectorFunction[I2,J2,S]):VectorFunction[Either[I,I2],Either[J,J2],S] = new BiMap(this,that)

  /* ---------------------------------------------------------------------------------------------------------------- */

  def feedback(x:Vector[J,S],y:Vector[I,S]):Vector[J,S]

  /* ---------------------------------------------------------------------------------------------------------------- */

  import linear.reindex._

  def apply(i:I):VectorFunction[Unit,J,S] = Extract.element(target,i) << this

  def getLeft[L,R,II<:Either[L,R] with I] : VectorFunction[L,J,S] =
    Extract.first(target.asPairSpace[L,R,II]) << this.asInstanceOf[VectorFunction[Either[L,R],J,S]]

  def getRight[L,R,II<:Either[L,R] with I] : VectorFunction[R,J,S] =
    Extract.second(target.asPairSpace[L,R,II]) << this.asInstanceOf[VectorFunction[Either[L,R],J,S]]

  def row[Outer,Inner,II<:(Outer,Inner) with I](i:Outer):VectorFunction[Inner,J,S] =
    Extract.row[Outer,Inner,S](target.asMatrixSpace,i) <<
      this.asInstanceOf[VectorFunction[(Outer,Inner),J,S]]

  def col[Outer,Inner,II<:(Outer,Inner) with I](j:Inner):VectorFunction[Outer,J,S] =
    Extract.col[Outer,Inner,S](target.asMatrixSpace,j) <<
      this.asInstanceOf[VectorFunction[(Outer,Inner),J,S]]

  def map(f:ScalarFunction[S]):VectorFunction[I,J,S] = new function.map.Map(target,f) << this

  def elemWise[J2](op:VectorFunction[Unit,Either[Unit,Unit],S])(
    that:VectorFunction[I,J2,S]):VectorFunction[I,Either[J,J2],S] =
    new ElemWise(target,op) << (this | that)

  def +[J2](that:VectorFunction[I,J2,S]):VectorFunction[I,Either[J,J2],S] = new Addition(target) << (this|that)

  def -[J2](that:VectorFunction[I,J2,S]):VectorFunction[I,Either[J,J2],S] = this + -that

  def o[J2](that:VectorFunction[I,J2,S]):VectorFunction[I,Either[J,J2],S] =
    ElementWiseMultiplication(target) << (this | that)

  def unary_-():VectorFunction[I,J,S] = new ScalarProduct(target,scalarSpace.fromInt(-1)) << this

  /* ---------------------------------------------------------------------------------------------------------------- */

  def s:VectorFunction[Unit,J,S] = new Sum(target) << this

  def <>[J2](that:VectorFunction[I,J2,S]):VectorFunction[Unit,Either[J,J2],S] = (this o that).s

  def ><[I2,J2](that:VectorFunction[I2,J2,S]):VectorFunction[(I,I2),Either[J,J2],S] =
    OuterProduct(this.target,that.target) << (this | that)


  def -*||[I2,J2](that:VectorFunction[(I,I2),J2,S]):VectorFunction[I2,Either[J,J2],S] =
    VectorMatrixProduct(this.target,that.target.asMatrixSpace[I,I2,(I,I2)].inner) << (this | that) //TODO is there some better way (getting rid of type parameters)?

  def =*|[IL,IR,IP<:(IL,IR) with I,J2](that:VectorFunction[IR,J2,S]):VectorFunction[IL,Either[J,J2],S] =  //TODO some way of getting rid of 'asInstanceOf'??
    MatrixVectorProduct(this.target.asMatrixSpace[IL,IR,IP].outer,that.target) <<
      (this.asInstanceOf[VectorFunction[(IL,IR),J,S]] | that)

  def =*||[IL,IR,IP<:(IL,IR) with I,I2,J2](that:VectorFunction[(IR,I2),J2,S]):VectorFunction[(IL,I2),Either[J,J2],S] = { //TODO try to simplify
    val thisSpace = this.target.asMatrixSpace[IL, IR, IP]
    val thatSpace = that.target.asMatrixSpace[IR, I2, (IR, I2)]
    val middleSpace = utilities.same(thisSpace.inner, thatSpace.outer)
    val thisTyped = this.asInstanceOf[VectorFunction[(IL, IR), J, S]]
    MatrixMatrixProduct(thisSpace.outer, middleSpace, thatSpace.inner) << (thisTyped | that)
  }

}

object VectorFunction {

  def serialize[I,J,S](f:VectorFunction[I,J,S]):Iterable[Byte] =
    Iterable(utilities.Serialization.version) ++ utilities.Serialization.ScalarSpace.serialize(f.scalarSpace) ++
    f.serialize

  def deserialize[I,J,S](bytes:Iterator[Byte]):VectorFunction[I,J,S] = {
    import utilities.Serialization._
    checkVersion(bytes.next())
    val scalarSpace = ScalarSpace.deserialize(Iterator(bytes.next()))
    if (scalarSpace != osiris.ScalarSpace[S]()) {
      throw new Exception(s"Trying to parse $scalarSpace as ${osiris.ScalarSpace[S]()}")
    }
    deserialize[I,J,S](scalarSpace.asInstanceOf[ScalarSpace[S]],bytes)
  }

  def deserialize[I,J,S](S:ScalarSpace[S], bytes:Iterator[Byte]):VectorFunction[I,J,S] = {
    import utilities.Serialization._
    (bytes.next() match {
      case Function.identity => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        new Identity(shape --> S)
      }
      case Function.constant => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        val c = (shape --> S).deserialize(bytes)
        new Constant(c)
      }
      case Function.deadInput => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        new DeadInput(shape --> S)
      }
      case Function.composition => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[VectorFunction[_,A,S]]
        val gg = g.asInstanceOf[VectorFunction[A,_,S]]
        ff << gg
      }
      case Function.composedLinear => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[LinearFunction[_,A,S]]
        val gg = g.asInstanceOf[LinearFunction[A,_,S]]
        ff << gg
      }
      case Function.linearComposeBilinear => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[LinearFunction[_,A,S]]
        val gg = g.asInstanceOf[BilinearFunction[A,_,_,S]]
        ff << gg
      }
      case Function.bilinearLeftComposeLinear => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain.asPairSpace.left,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[BilinearFunction[_,A,_,S]]
        val gg = g.asInstanceOf[LinearFunction[A,_,S]]
        ff <</ gg
      }
      case Function.bilinearRightComposeLinear => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain.asPairSpace.right,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[BilinearFunction[_,_,A,S]]
        val gg = g.asInstanceOf[LinearFunction[A,_,S]]
        ff <<\ gg
      }
      case Function.map => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        val f = osiris.function.ScalarFunction.deserialize(S,bytes)
        new osiris.function.map.Map(shape --> S,f)
      }
      case Function.rowMap => {
        val outerShape = osiris.shape.Shape.deserialize(bytes)
        type O = outerShape.Type
        val o = outerShape.asInstanceOf[shape.Shape[O]]
        val f = deserialize(S,bytes)
        new osiris.function.map.RowMap[O,f.target.shape.Type,f.domain.shape.Type,S](o-->S,f)
      }
      case Function.colMap => {
        val innerShape = osiris.shape.Shape.deserialize(bytes)
        type I = innerShape.Type
        val i = innerShape.asInstanceOf[shape.Shape[I]]
        val f = deserialize(S,bytes)
        new osiris.function.map.ColMap[f.target.shape.Type,f.domain.shape.Type,I,S](i-->S,f)
      }
      case Function.biMap => {
        val l = deserialize(S,bytes)
        val r = deserialize(S,bytes)
        new osiris.function.map.BiMap[l.target.shape.Type,l.domain.shape.Type,r.target.shape.Type,r.domain.shape.Type,S](l,r)
      }
      case Function.lMap => {
        val l = deserialize(S,bytes)
        val r = osiris.shape.Shape.deserialize(bytes)
        type R = r.Type
        val rr = r.asInstanceOf[shape.Shape[R]]
        new osiris.function.map.LMap[l.target.shape.Type,l.domain.shape.Type,R,S](l,rr --> S)
      }
      case Function.rMap => {
        val l = osiris.shape.Shape.deserialize(bytes)
        type L = l.Type
        val ll = l.asInstanceOf[shape.Shape[L]]
        val r = deserialize(S,bytes)
        new osiris.function.map.RMap[L,r.target.shape.Type,r.domain.shape.Type,S](ll --> S,r)
      }
      case Function.elemWise => {
        val targetShape = osiris.shape.Shape.deserialize(bytes)
        val f = deserialize(S,bytes)
        new osiris.function.map.ElemWise(targetShape-->S,f.asInstanceOf[VectorFunction[Unit,+[Unit,Unit],S]])
      }
      case Function.rowWise => {
        val outerShape = osiris.shape.Shape.deserialize(bytes)
        type O = outerShape.Type
        val o = outerShape.asInstanceOf[shape.Shape[O]]
        val f = deserialize(S,bytes)
        val domain = f.domain.asPairSpace
        type I = f.target.shape.Type
        type L = domain.left.shape.Type
        type R = domain.right.shape.Type
        new osiris.function.map.RowWise[O,I,L,R,S](o-->S,f.asInstanceOf[VectorFunction[I,+[L,R],S]])
      }
      case Function.colWise => {
        val innerShape = osiris.shape.Shape.deserialize(bytes)
        type J = innerShape.Type
        val i = innerShape.asInstanceOf[shape.Shape[J]]
        val f = deserialize(S,bytes)
        val domain = f.domain.asPairSpace
        type I = f.target.shape.Type
        type L = domain.left.shape.Type
        type R = domain.right.shape.Type
        new osiris.function.map.ColWise[I,L,R,J,S](i-->S,f.asInstanceOf[VectorFunction[I,+[L,R],S]])
      }
      case Function.rowMappedLinear => {
        val outerShape = osiris.shape.Shape.deserialize(bytes)
        val f = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        new osiris.function.linear.map.RowMappedLinear(outerShape-->S,f)
      }
      case Function.colMappedLinear => {
        val innerShape = osiris.shape.Shape.deserialize(bytes)
        val f = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        new osiris.function.linear.map.ColMappedLinear(innerShape-->S,f)
      }
      case Function.biMappedLinear => {
        val l = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        val r = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        new osiris.function.linear.map.BiMappedLinear(l,r)
      }
      case Function.lMappedLinear => {
        val l = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        val r = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.map.LMappedLinear(l,r-->S)
      }
      case Function.rMappedLinear => {
        val l = osiris.shape.Shape.deserialize(bytes)
        val r = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        new osiris.function.linear.map.RMappedLinear(l-->S,r)
      }
      case Function.rowWiseBilinear => {
        val outerShape = osiris.shape.Shape.deserialize(bytes)
        type O = outerShape.Type
        val o = outerShape.asInstanceOf[shape.Shape[O]]
        val f = deserialize(S,bytes)
        val domain = f.domain.asPairSpace
        type I = f.target.shape.Type
        type L = domain.left.shape.Type
        type R = domain.right.shape.Type
        new osiris.function.map.RowWise[O,I,L,R,S](o-->S,f.asInstanceOf[BilinearFunction[I,L,R,S]])
      }
      case Function.colWiseBilinear => {
        val innerShape = osiris.shape.Shape.deserialize(bytes)
        type J = innerShape.Type
        val i = innerShape.asInstanceOf[shape.Shape[J]]
        val f = deserialize(S,bytes)
        val domain = f.domain.asPairSpace
        type I = f.target.shape.Type
        type L = domain.left.shape.Type
        type R = domain.right.shape.Type
        new osiris.function.map.ColWise[I,L,R,J,S](i-->S,f.asInstanceOf[BilinearFunction[I,L,R,S]])
      }
      case Function.simpleLinear => {
        val shape = osiris.shape.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Product[_,_]]
        type A = shape.a.Type
        type B = shape.b.Type
        val space = (shape.asInstanceOf[osiris.shape.Product[A,B]] --> S)
        val k = space.deserialize(bytes)
        new osiris.function.linear.SimpleLinear[A,B,S](k.asMatrix)
      }
      case Function.addition => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.Addition(shape --> S)
      }
      case Function.sum => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.Sum(shape --> S)
      }
      case Function.rowSum => {
        val outer = osiris.shape.Shape.deserialize(bytes)
        val inner = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.RowSum(outer-->S,inner-->S)
      }
      case Function.colSum => {
        val outer = osiris.shape.Shape.deserialize(bytes)
        val inner = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.ColSum(outer-->S,inner-->S)
      }
      case Function.copy => {
        val domain = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.reindex.Copy(domain-->S)
      }
      case Function.fill => {
        val target = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.reindex.Fill(target-->S)
      }
      case Function.rowCopy => {
        val outer = osiris.shape.Shape.deserialize(bytes)
        val inner = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.reindex.RowCopy(outer-->S,inner-->S)
      }
      case Function.colCopy => {
        val outer = osiris.shape.Shape.deserialize(bytes)
        val inner = osiris.shape.Shape.deserialize(bytes)
        new osiris.function.linear.reindex.ColCopy(outer-->S,inner-->S)
      }
      case Function.permute => {
        val f = osiris.morphism.Morphism.deserialize(bytes)
        type I = f.domain.Type
        type J = f.target.Type
        new osiris.function.linear.reindex.Permute[I,J,S](
          f.domain-->S,
          f.target-->S,
          f.asInstanceOf[Isomorphism[I,J]]
        )
      }
      case Function.extract => {
        val f = osiris.morphism.Morphism.deserialize(bytes)
        type I = f.domain.Type
        type J = f.target.Type
        new osiris.function.linear.reindex.Extract[I,J,S](
          f.domain-->S,
          f.target-->S,
          f.asInstanceOf[osiris.morphism.Monomorphism[I,J]]
        )
      }
      case Function.pad => {
        val f = osiris.morphism.Morphism.deserialize(bytes)
        type I = f.domain.Type
        type J = f.target.Type
        new osiris.function.linear.reindex.Pad[J,I,S](
          f.target-->S,
          f.domain-->S,
          f.asInstanceOf[osiris.morphism.Monomorphism[I,J]]
        )
      }
      case Function.dft => {
        val shape = osiris.shape.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Range]
        new osiris.function.linear.DiscreteFourierTransform[S](shape-->S)
      }
      case Function.layeredBilinear => {
        val f = deserialize(S,bytes).asInstanceOf[BilinearFunction[_,_,_,S]]
        val g = deserialize(S,bytes).asInstanceOf[BilinearFunction[_,_,_,S]]
        f & g
      }
      case Function.multiplication => {
        new function.bilinear.Multiplication(S)
      }
      case Function.complexMultiplication => {
        new function.bilinear.ComplexMultiplication(S)
      }
      case Function.leftScalarProduct => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        new function.bilinear.LeftScalarProduct(shape --> S)
      }
      case Function.rightScalarProduct => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        new function.bilinear.RightScalarProduct(shape --> S)
      }
      case Function.elementWiseMultiplication => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        new function.bilinear.ElementWiseMultiplication(shape --> S)
      }
      case Function.innerProduct => {
        val shape = osiris.shape.Shape.deserialize(bytes)
        new function.bilinear.InnerProduct(shape --> S)
      }
      case Function.outerProduct => {
        val a = osiris.shape.Shape.deserialize(bytes)
        val b = osiris.shape.Shape.deserialize(bytes)
        new function.bilinear.OuterProduct(a-->S,b-->S)
      }
      case Function.vectorMatrixProduct => {
        val a = osiris.shape.Shape.deserialize(bytes)
        val b = osiris.shape.Shape.deserialize(bytes)
        new function.bilinear.VectorMatrixProduct(a-->S,b-->S)
      }
      case Function.matrixMatrixProduct => {
        val a = osiris.shape.Shape.deserialize(bytes)
        val b = osiris.shape.Shape.deserialize(bytes)
        val c = osiris.shape.Shape.deserialize(bytes)
        new function.bilinear.MatrixMatrixProduct(a-->S,b-->S,c-->S)
      }
      case Function.matrixVectorProduct => {
        val a = osiris.shape.Shape.deserialize(bytes)
        val b = osiris.shape.Shape.deserialize(bytes)
        new function.bilinear.MatrixVectorProduct(a-->S,b-->S)
      }
      case Function.convolution => {
        val left = osiris.shape.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Range]
        val right = osiris.shape.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Range]
        val target = osiris.shape.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Range]
        implicit val tag = S.tag
        new function.bilinear.Convolution[S](left-->S,right-->S,target-->S)
      }
    }).asInstanceOf[VectorFunction[I,J,S]]
  }

}

case class Composition[I,K,J,S](outer:VectorFunction[I,K,S], inner:VectorFunction[K,J,S])
  extends VectorFunction[I,J,S] {

  val domain = inner.domain
  val middle = utilities.same(outer.domain, inner.target)
  val target = outer.target

  override def toString(): String = s"($outer < $inner)"

  def serialize:Iterable[Byte] =
    Iterable(utilities.Serialization.Function.composition) ++ outer.serialize ++ inner.serialize

  def apply(x: Vector[J, S]): Vector[I, S] = outer(inner(x))

  def feedback(x:Vector[J,S],y:Vector[I,S]):Vector[J,S] = inner.feedback(x,outer.feedback(inner(x),y))

}