// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.map

import osiris.function.{ScalarFunction, VectorFunction}
import osiris.utilities
import osiris.vector._
import osiris.vector.space.VectorSpace

class Map[I,S](val domain:VectorSpace[I,S], f:ScalarFunction[S])
  extends VectorFunction[I,I,S] {

  val target = domain

  override def toString():String = s"Map $domain,$f"

  def serialize:Iterable[Byte] =
    Iterable(utilities.Serialization.Function.map) ++
    domain.shape.serialize ++ f.serialize

  def apply(x:Vector[I,S]):Vector[I,S] = x.map(f)

  def feedback(x:Vector[I,S],y:Vector[I,S]):Vector[I,S] = new Map(domain,f.derivative)(x) o y

}

