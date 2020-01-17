// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.linear

import osiris.utilities
import osiris.vector.Vector
import osiris.vector.space.VectorSpace

class ScalarProduct[I,S](val domain:VectorSpace[I,S],k:S) extends LinearFunction[I,I,S] {

  val target = domain

  def serialize:Iterable[Byte] =
    Iterable(utilities.Serialization.Function.scalarProduct) ++
    domain.shape.serialize

  def apply(x:Vector[I,S]):Vector[I,S] = x*k

  def feedback = this

}
