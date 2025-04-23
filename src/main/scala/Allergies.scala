import Allergen.Strawberries

enum Allergen(val score: Int):
   case Eggs extends Allergen(1)
   case Peanuts extends Allergen(2)
   case Shellfish extends Allergen(4)
   case Strawberries extends Allergen(8)
   case Tomatoes extends Allergen(16)
   case Chocolate extends Allergen(32)
   case Pollen extends Allergen(64)
   case Cats extends Allergen(128)

object Allergies:
   def list(n: Int): List[Allergen] =
      Allergen.values.zip(n.toBinaryString.reverse).filter(_._2 == '1').map(_._1).toList

   def allergicTo(a: Allergen, n: Int): Boolean =
      Allergen.values.map(_.score).contains(n - a.score) || n == a.score && n != 0
