package scala

object Sorter {

  def scoreWord(word: String): Int = {
    def scoreLetter(char: Char): Int = {
      if ("QZ".contains(char)) {
        10
      } else if ("JX".contains(char)) {
        8
      } else if (char == 'K') {
        5
      } else if ("FHVWY".contains(char)) {
        4
      } else if ("BCMP".contains(char)) {
        3
      } else if ("DG".contains(char)) {
        2
      } else 1
    }

    word.toVector.map(scoreLetter).sum
  }

  case class ScrabbleWord(word: String) {
    def score():Int ={
      scoreWord(word)
    }
  }

  //Puts the words in descending order by length
  def sortByLength(words: Vector[String]): Vector[String] = {
    words.sortWith((a: String, b: String) => a.length >= b.length)
  }

  //Takes the list of words that have been sorted by length and turns it into
  //A vector of vectors of words that are all the same length
  def splitByLength(words: Vector[String], lists: Vector[Vector[String]], list: Vector[String],
                    currentLength: Int): Vector[Vector[String]] = {
    if (words.isEmpty) {
      lists :+ list
    } else if (words.head.length == currentLength) {
      splitByLength(words.tail, lists, list :+ words.head, currentLength)
    } else {
      splitByLength(words.tail, lists :+ list, Vector(words.head), words.head.length)
    }
  }

  //Returns true if the first word belongs before the second in alphabetical order
  def isAlphabetic(a: String, b: String): Boolean = {
    if (a.isEmpty) {
      true
    } else if (b.isEmpty) {
      false
    } else if (a.head < b.head) {
      true
    } else if (a.head == b.head) {
      isAlphabetic(a.tail, b.tail)
    } else {
      false
    }
  }

  //Sorts the words by length then dictionary
  def sortLenAlpha(words: Vector[String]): Vector[String] = {
    splitByLength(sortByLength(words), Vector(), Vector(), words.head.length)
            .map(list => list.sortWith(isAlphabetic).:+("")).flatten
  }

  //Sorts and labels words by their scrabble score
  def sortScrabble(words: Vector[String]): Vector[String] = {
    words.map(w => new ScrabbleWord(w)).sortWith((a, b) => a.score >= b.score)
            .map(w => w.score + "-" + w.word)
  }

}
