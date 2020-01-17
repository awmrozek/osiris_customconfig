// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.utilities

import osiris.{F32, F64, ScalarSpace}

object Serialization {

  val version:Byte = 1


  object Primitives {

    def serializeInt(i:Int):Iterable[Byte] = java.nio.ByteBuffer.allocate(4).putInt(i).array()
    def deserializeInt(bytes:Iterator[Byte]):Int = {
      val arr = new Array[Byte](4)
      for (i <- 0 until 4) {arr(i) = bytes.next()}
      java.nio.ByteBuffer.wrap(arr).getInt()
    }

    def serializeFloat(f:Float):Iterable[Byte] = java.nio.ByteBuffer.allocate(4).putFloat(f).array()
    def deserializeFloat(bytes:Iterator[Byte]):Float =  {
      val arr = new Array[Byte](4)
      for (i <- 0 until 4) {arr(i) = bytes.next()}
      java.nio.ByteBuffer.wrap(arr).getFloat()
    }

    def serializeDouble(d:Double):Iterable[Byte] = java.nio.ByteBuffer.allocate(8).putDouble(d).array()
    def deserializeDouble(bytes:Iterator[Byte]):Double =  {
      val arr = new Array[Byte](8)
      for (i <- 0 until 8) {arr(i) = bytes.next()}
      java.nio.ByteBuffer.wrap(arr).getDouble()
    }

  }

  object Representation {

    val elems:Byte = 1
    val reindex:Byte = 2

  }

  object Shape {

    val sum:Byte= 1
    val product:Byte = 2

    val empty:Byte = 3
    val single:Byte = 4
    val range:Byte = 5

  }

  object Index {

    val left :Byte = 1
    val right:Byte = 2

  }

  object Morphism {

    val id:Byte = 1
    val unit:Byte = 2
    val absurd:Byte = 3
    val constant:Byte = 4
    val table:Byte = 5

    val composition:Byte = 10

    val lmap:Byte = 15
    val rmap:Byte = 16
    val bimap:Byte = 17

    val sum:Byte = 20
    val product:Byte = 21

    val equal:Byte = 30

    val and:Byte = 35
    val or:Byte = 36
    val xor:Byte = 37

    val translate:Byte = 40
    val inv:Byte = 41

    val commute:Byte = 50

    val leftAssoc:Byte = 51
    val rightAssoc:Byte = 52

    val putLeft:Byte = 53
    val putRight:Byte = 54
    val getLeft:Byte = 55
    val getRight:Byte = 56

    val distrLeft:Byte = 57
    val distrRight:Byte = 58
    val extractLeft:Byte = 59
    val extractRight:Byte = 60

    val join:Byte = 70
    val left:Byte = 71
    val right:Byte = 72

    val spookLeft:Byte = 80
    val spookRight:Byte = 81
    val bustLeft:Byte = 82
    val bustRight:Byte = 83

    val first:Byte = 84
    val second:Byte = 85

    val firstWith:Byte = 86
    val secondWith:Byte = 87

    val copy:Byte = 88

  }

  object ScalarSpace {

    val f32:Byte = 5
    val f64:Byte = 6

    def serialize(s:ScalarSpace[_]):Iterable[Byte] = Iterable(s match {
      case F32 => f32
      case F64 => f64
    })

    def deserialize(s:Iterator[Byte]):ScalarSpace[_] = s.next() match {
      case this.f32 => F32
      case this.f64 => F64
    }

  }

  object ScalarFunction {

    val id:Byte = 1
    val const:Byte = 2

    val composition:Byte = 10

    val add:Byte = 21
    val mul:Byte = 22
    val sub:Byte = 23
    val div:Byte = 24
    val neg:Byte = 25
    val inv:Byte = 26
    val pow:Byte = 27
    val mon:Byte = 28

    val exp:Byte = 41
    val log:Byte = 42

    val sin:Byte = 51
    val cos:Byte = 52
    val tan:Byte = 53
    val asin:Byte = 54
    val acos:Byte = 55
    val atan:Byte = 56

    val abs:Byte = 71
    val heaviside:Byte = 72

  }

  object Function {

    val identity:Byte = 1
    val constant:Byte = 2
    val deadInput:Byte = 3

    val composition:Byte = 10
    val composedLinear:Byte = 11
    val linearComposeBilinear:Byte = 12
    val bilinearLeftComposeLinear:Byte = 13
    val bilinearRightComposeLinear:Byte = 14

    val map:Byte = 20
    val rowMap:Byte = 21
    val colMap:Byte = 22
    val biMap:Byte = 23
    val lMap:Byte = 24
    val rMap:Byte = 25

    val elemWise:Byte = 26
    val rowWise:Byte = 27
    val colWise:Byte = 28

    val rowMappedLinear:Byte = 30
    val colMappedLinear:Byte = 31
    val biMappedLinear:Byte = 32
    val lMappedLinear:Byte = 33
    val rMappedLinear:Byte = 34

    val rowWiseBilinear:Byte = 35
    val colWiseBilinear:Byte = 36

    val simpleLinear:Byte = 40
    val scalarProduct:Byte = 41

    val addition:Byte = 45
    val sum:Byte = 46
    val rowSum:Byte = 47
    val colSum:Byte = 48

    val copy:Byte = 51
    val fill:Byte = 52
    val rowCopy:Byte = 53
    val colCopy:Byte = 54

    val permute:Byte = 61
    val extract:Byte = 62
    val pad:Byte = 63

    val dft:Byte = 67

    val layeredBilinear:Byte = 70
    val multiplication:Byte = 71
    val complexMultiplication:Byte = 72
    val leftScalarProduct:Byte = 73
    val rightScalarProduct:Byte = 74
    val elementWiseMultiplication:Byte = 75
    val innerProduct:Byte = 76
    val outerProduct:Byte = 77
    val vectorMatrixProduct:Byte = 78
    val matrixMatrixProduct:Byte = 79
    val matrixVectorProduct:Byte = 80
    val convolution:Byte = 8

  }

  def checkVersion(version:Byte):Unit = if(version > Serialization.version) {throw new VersionException(version)}

  class VersionException(version:Byte)
    extends Exception(
      s"Trying to parse serialization of version $version. Only supports versions up to ${Serialization.version}"
    )


}
