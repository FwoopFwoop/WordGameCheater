package scala

object Sorter {
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

}
