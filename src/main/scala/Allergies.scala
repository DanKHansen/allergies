import Allergen.Strawberries

enum Allergen(val score: Int):
   case Eggs extends Allergen(1) // 1
   case Peanuts extends Allergen(2) // 10
   case Shellfish extends Allergen(4) // 100
   case Strawberries extends Allergen(8) // 1.000
   case Tomatoes extends Allergen(16) // 10.000
   case Chocolate extends Allergen(32) // 100.000
   case Pollen extends Allergen(64) // 1.000.000
   case Cats extends Allergen(128) // 10.000.000

object Allergies:
   def list(n: Int): List[Allergen] =
      println(s"Scoring $n ('${n.toBinaryString}'), which allergens are you allergic to?")
      if n == 0 then Nil
      else if n == Allergen.values.map(_.score).sum then Allergen.values.toList
      else Allergen.values.filter(i => i.score == n).toList

   def allergicTo(a: Allergen, n: Int): Boolean =
      if n == 0 then false else Allergen.values.map(_.score).contains(n - a.score) || n == a.score
