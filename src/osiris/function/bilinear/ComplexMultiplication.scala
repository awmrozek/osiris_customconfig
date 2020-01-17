// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.function.bilinear

import osiris.morphism.{product, sum}
import osiris.vector.space.VectorSpace
import osiris.vector.{Matrix, Pair, Vector}
import osiris.{+, I, ScalarSpace, utilities}

class ComplexMultiplication[S](space:ScalarSpace[S])
  extends BilinearFunction[Either[Unit,Unit],Either[Unit,Unit],Either[Unit,Unit],S] {

  val left = (I + I) --> scalarSpace
  val right = (I + I) --> scalarSpace

  val target = (I + I) --> scalarSpace

  def serialize:Iterable[Byte] = Iterable(utilities.Serialization.Function.complexMultiplication)

  override def apply(x:Vector[Either[Either[Unit,Unit],Either[Unit,Unit]],S]):Pair[Unit,Unit,S] = {
    val xp = x.asPair[+[Unit,Unit],+[Unit,Unit],+[+[Unit,Unit],+[Unit,Unit]]]
    val l = xp.left.asPair[Unit,Unit,+[Unit,Unit]]
    val r = xp.right.asPair[Unit,Unit,+[Unit,Unit]]
    Pair(l.left o r.left - l.right o r.right, l.left o r.right + l.right o l.left)
  }

  def apply[II,LL,RR](iiSpace:VectorSpace[II,S],llSpace:VectorSpace[LL,S],rrSpace:VectorSpace[RR,S],
                      l:Matrix[Either[Unit,Unit],LL,S],r:Matrix[Either[Unit,Unit],RR,S],
                      op:(Vector[LL,S],Vector[RR,S]) => Vector[II,S]):Matrix[Either[Unit,Unit],II,S] = {

    val ll = l.reIndex(llSpace+llSpace,
      product.rightExtract[Unit,Unit,LL](I,I,llSpace.shape) o
        sum.bimap[LL,(Unit,LL),LL,(Unit,LL)](product.putRight[LL](llSpace.shape),product.putRight[LL](llSpace.shape))
    ).asPair[LL,LL,+[LL,LL]]
    val rr = r.reIndex(rrSpace+rrSpace,
      product.rightExtract[Unit,Unit,RR](I,I,rrSpace.shape) o
        sum.bimap[RR,(Unit,RR),RR,(Unit,RR)](product.putRight[RR](rrSpace.shape),product.putRight[RR](rrSpace.shape))
    ).asPair[RR,RR,+[RR,RR]]


    Pair(op(ll.left,rr.left)-op(ll.right,rr.right),op(ll.left,rr.right)+op(ll.right,rr.left)).reIndex(target*iiSpace,
      sum.bimap[(Unit,II),II,(Unit,II),II](product.getRight[II](iiSpace.shape),product.getRight[II](iiSpace.shape)) o
        product.rightDistr[Unit,Unit,II](I,I,iiSpace.shape)
    ).asMatrix
  }

  def leftFeedback = this.permuteTarget(sum.commute(I,I)).permuteRight(sum.commute(I,I))

  def rightFeedback = this.permuteTarget(sum.commute(I,I)).permuteRight(sum.commute(I,I))

}
