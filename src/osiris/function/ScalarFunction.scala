// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function

import osiris._

trait ScalarFunction[S] extends (S => S) {

  val space:ScalarSpace[S]

  def serialize:Iterable[Byte]

  def apply(x:S):S

  def <<(that:ScalarFunction[S]):ScalarFunction[S] = new Composition(this,that)

  def +(that:ScalarFunction[S]):ScalarFunction[S] = new Plus(this,that)

  def -(that:ScalarFunction[S]):ScalarFunction[S] = new Minus(this,that)

  def *(that:ScalarFunction[S]):ScalarFunction[S] = new Times(this,that)

  def /(that:ScalarFunction[S]):ScalarFunction[S] = new Div(this,that)

  def ^(that:ScalarFunction[S]):ScalarFunction[S] = new Power(this,that)

  def ^(k:Int):ScalarFunction[S] = Monomial(space,k) << this

  def unary_- :ScalarFunction[S] = new Neg(this)

  def inv:ScalarFunction[S] = new Inv(this)

  def derivative:ScalarFunction[S]

  class Composition private[ScalarFunction] (outer:ScalarFunction[S],inner:ScalarFunction[S])
    extends ScalarFunction[S] {

    override def toString():String = s"($outer << $inner)"

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.ScalarFunction.composition) ++ outer.serialize ++ inner.serialize

    val space = utilities.same(outer.space,inner.space)

    def apply(x:S):S = outer(inner(x))

    def derivative:ScalarFunction[S] = (outer.derivative << inner) * inner.derivative

  }

  class Plus private[ScalarFunction]
  (f:ScalarFunction[S],g:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"($f + $g)"

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.ScalarFunction.add) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = space.+(f(x),g(x))

    def derivative: ScalarFunction[S] = f.derivative + g.derivative

  }

  class Minus private[ScalarFunction] (f:ScalarFunction[S],g:ScalarFunction[S])
    extends ScalarFunction[S] {

    override def toString():String = s"($f - $g)"

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.ScalarFunction.sub) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = space.-(f(x),g(x))

    def derivative: ScalarFunction[S] = f.derivative - g.derivative

  }

  class Neg private[ScalarFunction] (f:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"-$f" //u- stands for unary minus. Maybe find better name?

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.ScalarFunction.neg) ++ f.serialize

    val space = f.space

    def apply(x:S):S = space.neg(f(x))

    def derivative: ScalarFunction[S] = -f.derivative

  }

  class Times private[ScalarFunction] (f:ScalarFunction[S],g:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"($f * $g)"

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.ScalarFunction.mul) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = space.*(f(x),g(x))

    def derivative: ScalarFunction[S] = f.derivative * g + f * g.derivative

  }

  class Div private[ScalarFunction] (f:ScalarFunction[S],g:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"($f/$g)"

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.ScalarFunction.div) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = {
      val a = f(x)
      val b = g(x)
      if(a == space.zero && b == space.zero) {(f.derivative/g.derivative)(x)} else {space./(a,b)}
    }

    def derivative: ScalarFunction[S] = (f.derivative * g - f * g.derivative)/(g*g)

  }

  class Inv private[ScalarFunction] (f:ScalarFunction[S]) extends ScalarFunction[S] {

    override def toString():String = s"(1/$f)"

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.ScalarFunction.inv) ++ f.serialize

    val space = f.space

    def apply(x:S):S = space.inv(f(x))

    def derivative: ScalarFunction[S] = -f.derivative/(f*f)

  }

  class Power private[ScalarFunction] (f: ScalarFunction[S], g: ScalarFunction[S])
    extends ScalarFunction[S] {

    override def toString():String = s"($f^$g)"

    def serialize:Iterable[Byte] =
      Iterable(utilities.Serialization.ScalarFunction.pow) ++ f.serialize ++ g.serialize

    val space = utilities.same(f.space,g.space)

    def apply(x:S):S = space.^(f(x),g(x))

    def derivative: ScalarFunction[S] = f.derivative * g * (f ^ (g - Const(space.one))) +
      g.derivative * (Ln(space) << f) * (f ^ g)

  }

}

object ScalarFunction {

  def serialize(f:ScalarFunction[_]):Iterable[Byte] =
    Iterable(utilities.Serialization.version) ++ utilities.Serialization.ScalarSpace.serialize(f.space) ++ f.serialize

  def deserialize[S](f:Iterator[Byte]):ScalarFunction[S] = {
    utilities.Serialization.checkVersion(f.next())
    val space = utilities.Serialization.ScalarSpace.deserialize(f)
    if (space.isInstanceOf[ScalarSpace[S]]) {
      deserialize(space.asInstanceOf[ScalarSpace[S]],f)
    } else {
      throw new Exception(s"Trying to parse $space as ${ScalarSpace[S]()}")
    }
  }

  def deserialize[S](space: ScalarSpace[S], bytes: Iterator[Byte]):ScalarFunction[S] = {
    import utilities.Serialization.{ScalarFunction => S}
    bytes.next() match {
      case S.id => Id(space)
      case S.const => Const(space.deserialize(bytes))
      case S.composition => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f << g
      }
      case S.add => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f + g
      }
      case S.mul => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f * g
      }
      case S.sub => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f - g
      }
      case S.div => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f / g
      }
      case S.pow => {
        val f = deserialize(space,bytes)
        val g = deserialize(space,bytes)
        f ^ g
      }
      case S.neg => {
        val f = deserialize(space,bytes)
        -f
      }
      case S.inv => {
        val f = deserialize(space,bytes)
        f.inv
      }
      case S.mon => {
        val k = utilities.Serialization.Primitives.deserializeInt(bytes)
        Monomial(space,k)
      }
      case S.exp => Exp(space)
      case S.log => Ln(space)
      case S.sin => Sin(space)
      case S.cos => Cos(space)
      case S.tan => Tan(space)
      case S.asin => ArcSin(space)
      case S.acos => ArcCos(space)
      case S.atan => ArcTan(space)
      case S.abs => Abs(space)
      case S.heaviside => Heaviside(space)
    }
  }

}

class Monomial[S] private (val space: ScalarSpace[S], k: Int) extends ScalarFunction[S] {

  override def toString():String = s"_^$k"

  def serialize:Iterable[Byte] =
    Iterable(utilities.Serialization.ScalarFunction.mon) ++ utilities.Serialization.Primitives.serializeInt(k)

  def apply(x:S):S = space.^(x,k)

  def derivative: ScalarFunction[S] = Const(space.fromInt(k)) * Monomial(space,k-1)

}

object Monomial {

  def apply[S](space:ScalarSpace[S],k:Int):ScalarFunction[S] =
    if (k == 0) {Const(space.one)}
    else if (k == 1) {Id(space)}
    else {new Monomial[S](space,k)}

}

class Const[S] private (val c:S) extends ScalarFunction[S] {

  val space = ScalarSpace(c)

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.const)

  override def toString():String = s"const $c"

  def apply(x:S):S = c

  def derivative: ScalarFunction[S] = new Const(space.zero)

}

object Const {

  def apply[S](c:S) = new Const(c)

}

class Id[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "id"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.id)

  def apply(x:S):S = x

  def derivative: ScalarFunction[S] = Const(space.one)

}

object Id {

  def apply[S](sSpace:ScalarSpace[S]) = new Id(sSpace)

}

class Exp[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "exp"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.exp)

  def apply(x:S):S = space.exp(x)

  def derivative = Exp[S](space)

}

object Exp {

  def apply[S](space:ScalarSpace[S]) = new Exp(space)

}

class Ln[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "ln"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.log)

  def apply(x:S):S = space.ln(x)

  def derivative = Id(space).inv

}

object Ln {

  def apply[S](space:ScalarSpace[S]) = new Ln(space)

}

class Sin[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "sin"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.sin)

  def apply(x:S):S = space.sin(x)

  def derivative = Cos(space)

}

object Sin {

  def apply[S](space:ScalarSpace[S]) = new Sin(space)

}

class Cos[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "cos"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.cos)

  def apply(x:S):S = space.cos(x)

  def derivative = -Sin(space)

}

object Cos {

  def apply[S](space:ScalarSpace[S]) = new Cos(space)

  val R = F64

  val y = Sin(R)(4.0)

}

class Tan[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "tan"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.tan)

  def apply(x:S):S = space.tan(x)

  def derivative = (Cos(space)*Cos(space)).inv

}

object Tan {

  def apply[S](space:ScalarSpace[S]) = new Tan(space)

}


class ArcSin[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "arcsin"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.asin)

  def apply(x:S):S = space.arcsin(x)

  def derivative = ((Const(space.one) - Id(space)*Id(space))^Const(space.fromDouble(0.5))).inv

}

object ArcSin {

  def apply[S](space:ScalarSpace[S]) = new ArcSin(space)

}

class ArcCos[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "arccos"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.acos)

  def apply(x:S):S = space.arccos(x)

  def derivative = -ArcSin(space).derivative

}

object ArcCos {

  def apply[S](space:ScalarSpace[S]) = new ArcCos(space)

}

class ArcTan[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "arctan"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.atan)

  def apply(x:S):S = space.arctan(x)

  def derivative = (Const(space.one) + Id(space)*Id(space)).inv

}

object ArcTan {

  def apply[S](space:ScalarSpace[S]) = new ArcTan(space)

}

class Abs[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "abs"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.abs)

  def apply(x:S):S = space.abs(x)

  def derivative = Const(space.fromDouble(2))* Heaviside(space) - Const(space.one)

}

object Abs {

  def apply[S](space:ScalarSpace[S]) = new Abs(space)

}


class Heaviside[S] private (val space:ScalarSpace[S]) extends ScalarFunction[S] {

  override def toString() = "theta"

  def serialize: Iterable[Byte] = Iterable(utilities.Serialization.ScalarFunction.heaviside)

  //Derivative is not distributional!!

  def apply(x:S):S = space.heaviside(x)

  def derivative = Const(space.zero)

}

object Heaviside {

  def apply[S](space:ScalarSpace[S]) = new Heaviside(space)

}