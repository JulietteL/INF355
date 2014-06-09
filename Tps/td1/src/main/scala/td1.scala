package fr.enst.plnc2014.td1

class ExtSeq[T](s : Seq[T])
{
  def any(f: T => Boolean): Boolean = {if(s.isEmpty)
{false}
else
f(s.head) || new ExtSeq(s.tail).any(f) 
}

  def all(f: T => Boolean): Boolean = {if(s.isEmpty)
{true}
else
f(s.head) && new ExtSeq(s.tail).all(f)}
}

object ExtSeq
{
  implicit def toExtSeq[T](seq : Seq[T]) : ExtSeq[T] = new ExtSeq[T](seq)
}

class ExtCond(cond: => Boolean)
{
  def doWhile(f: => Any): Unit = 
  {
    f
    if(cond)
      doWhile(f)
  }
}

object ExtCond
{
  implicit def toExtCond(cond: => Boolean) : ExtCond = new ExtCond(cond)
}

case class Complex(re: Double, im: Double)
{
  override def toString =
    if(im == 0.0)
      re.toString
    else if (re == 0.0)
      im+"i"
    else if (im > 0.0)
      re+"+"+im+ "i"
    else
      re.toString  + im.toString+ "i"

  def reciprocal: Complex = new Complex(re, -im)

  def abs: Double = math.sqrt(re*re + im*im)

  def +(c : Complex): Complex = new Complex(re + c .re, im + c .im)

  def -(c : Complex): Complex = new Complex(re - c .re, im - c .im)

  def *(c : Complex): Complex = new Complex(re*c.re - im*c.im, im*c.re + re*c.im)

// Pour éviter une récursion infinie lors de la division complexe.
  def /(d : Double): Complex = new Complex(re/d, im/d)

  def /(c : Complex): Complex = this*c.reciprocal/(c.re*c.re +c.im*c.im)

}

object Complex
{
  implicit def toComplex(re: Double, im: Double): Complex = new Complex(re, im)
  implicit def toComplex(re: Double) : Complex = new Complex (re, 0.0)
}

object TD1 {

  // Placer ici le code de isOdd & friends

  def isOdd(x: Int): Boolean = x.abs % 2 == 1
  def isEven(x: Int): Boolean = !isOdd(x)

  def myWhile(cond : => Boolean, f: => Any): Unit =
  {
    if(cond)
    {
      f
      myWhile(cond, f)
    }
  }

}

object Main extends App {

  import TD1._

  // Placer ici le code à exécuter

}
