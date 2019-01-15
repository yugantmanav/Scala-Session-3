package acadgild.calculator

object Rational {
  def main(args: Array[String]) {
    var ration = new RationalCalculator1("2/3", "*", "1/4")
    //var ration = new RationalCalculator1(2, "*", 3)
  }
}

class RationalCalculator1() {
  def this(op1: String, sign: String, op2: String) {
    this()
    println("Auxiliary Rational")
    var x = op1
    var y = op2
    var xArray: Array[String] = x.split("/")

    var xnumerator = xArray(0)
    var xdenominator = xArray(1)

    var yArray: Array[String] = y.split("/")

    var ynumerator = yArray(0)
    var ydenominator = yArray(1)

    def gcd(a: Int, b: Int): Int = {
      if (b == 0) a else gcd(b, a % b)
    }

    var g1 = gcd(xnumerator.toInt, xdenominator.toInt);
    var xnum = xnumerator.toInt / g1;
    var xden = xdenominator.toInt / g1;

    var g2 = gcd(ynumerator.toInt, ydenominator.toInt);
    var ynum = ynumerator.toInt / g2;
    var yden = ydenominator.toInt / g2;

    def calFraction(num: Int, den: Int, g: Int): String = {
      var resnum = num / g
      var resden = den / g

      return (resnum + "/" + resden)
    }

    if (sign.equals("+")) {
      add(xnum, xden, ynum, yden)
    } else if (sign.equals("-")) {
      subtract(xnum, xden, ynum, yden)
    } else if (sign.equals("*")) {
      multiply(xnum, xden, ynum, yden)
    } else if (sign.equals("/")) {
      divide(xnum, xden, ynum, yden)
    }

    def multiply(xnum: Int, xden: Int, ynum: Int, yden: Int) = {
      var num = xnum * ynum
      var den = xden * yden
      var gcd1 = gcd(num, den)
      var res = calFraction(num, den, gcd1)
      println(xnum + "/" + xden + " * " + ynum + "/" + yden + " = " + res)
    }

    def add(xnum: Int, xden: Int, ynum: Int, yden: Int) = {
      var num = (xnum * yden) + (xden * ynum)
      var den = xden * yden
      var gcd1 = gcd(num, den)
      var res = calFraction(num, den, gcd1)
      println(xnum + "/" + xden + " + " + ynum + "/" + yden + " = " + res)
    }

    def subtract(xnum: Int, xden: Int, ynum: Int, yden: Int) = {
      var num = (xnum * yden) - (xden * ynum)
      var den = xden * yden
      var gcd1 = gcd(num, den)
      var res = calFraction(num, den, gcd1)
      println(xnum + "/" + xden + " - " + ynum + "/" + yden + " = " + res)
    }

    def divide(xnum: Int, xden: Int, ynum: Int, yden: Int) = {
      var num = (xnum * yden)
      var den = xden * ynum
      var gcd1 = gcd(num, den)
      var res = calFraction(num, den, gcd1)
      println(xnum + "/" + xden + " / " + ynum + "/" + yden + " = " + res)
    }
  }

  def this(op1: Int, sign: String, op2: Int) {
    this()

    if (sign.equals("+")) {
      add(op1, op2)
    } else if (sign.equals("-")) {
      subtract(op1, op2)
    } else if (sign.equals("*")) {
      multiply(op1, op2)
    } else if (sign.equals("/")) {
      divide(op1, op2)
    }

    def add(op1: Int, op2: Int) = {
      var res = op1 + op2;
      println(res)
    }
    def subtract(op1: Int, op2: Int) = {
      var res = op1 - op2
      println(res)
    }
    def multiply(op1: Int, op2: Int) = {
      var res = op1 * op2
      println(res)
    }
    def divide(op1: Int, op2: Int) = {
      var res = op1 / op2
      println(res)
    }
  }
}
