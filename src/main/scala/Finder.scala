package scala

import scala.io.Source

object Finder {
  def matchingWords(letters: Vector[Char]): Vector[String] = {
    def isMadeFromLetters(word: String): Boolean = {
      def loop(letters: Vector[Char], word: Vector[Char]): Boolean = {
        if (word.isEmpty) {
          true
        } else if (letters.isEmpty) {
          false
        } else if (letters.head == word.head) {
          loop(letters.tail, word.tail)
        } else {
          loop(letters.tail, word)
        }
      }

      loop(letters.sortWith((a, b) => a < b), word.toVector.sortWith((a, b) => a < b))
    }

    Source.fromFile("C:\\Users\\zacha\\Code\\CrosswordCookieGenerator\\src\\main\\txt\\words.txt")
            .getLines().toVector.filter(isMadeFromLetters)
  }

}