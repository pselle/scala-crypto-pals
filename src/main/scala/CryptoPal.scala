/**
 * Created by pamselle on 3/31/15.
 */
object CryptoPal extends App {
    def hex2Byte(input: String ): Array[Byte] = {
      val len = input.length
      val data = new Array[Byte](len / 2)
      var i = 0
      while(i < len) {
        //    println(Character.digit(input.charAt(i), 16) << 4)
        //    println(Character.digit(input.charAt(i+1), 16))
        val b: Int = (Character.digit(input.charAt(i), 16) << 4) +
          Character.digit(input.charAt(i + 1), 16)
        //    println(b)
        data(i/2) = b.asInstanceOf[Byte]
        //    println(b.toBinaryString)
        i += 2
      }
      data
    }

    println(hex2Byte("49").mkString(", "))
    println(hex2Byte("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d").mkString(", "))

    def byte2Base64(input: Array[Byte]) = {
      val base64Code: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
        "abcdefghijklmnopqrstuvwxyz" + "0123456789" + "+/"

      val result = new Array[Char]((input.length * 4) / 3)
      println(input.length)
      var i = 0
      while (i < input.length) {
        result(i) = base64Code((input(i) & 252) >> 2)
        result(i + 1) = base64Code(((input(i) & 3) << 4) + ((input(i + 1) & 240) >> 4))
        result(i + 2) = base64Code(((input(i + 1) & 15) << 2) + ((input(i + 2) & 192) >> 6))
        result(i + 3) = base64Code(input(i + 2) & 63)
        i += 3
      }
      result.mkString("")
    }
}
