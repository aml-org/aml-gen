import org.scalacheck.Gen

object Main extends App {
  println("Hello World")

  val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'U')
  vowel.sample.foreach(println)
}
