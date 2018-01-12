package scala

import scala.Main.Flag.Flag

object Main {

  object Flag extends Enumeration {
    type Flag = Value
    val Scrabble = Value
  }

  def sanitize(letters: String): Vector[Char] = {
    letters.toUpperCase.filter(l => l.isLetter).toVector
  }

  def parseLetters(args: Array[String]): Vector[Char] = {
    if (args.isEmpty) {
      print("Enter letters: ")
      sanitize(scala.io.StdIn.readLine)
    } else {
      sanitize(args.head)
    }
  }

  def parseFlags(args: Array[String]): Option[Vector[Flag]] = {
    def flagFromString(arg: String): Option[Flag] = {
      if (arg.equalsIgnoreCase("scrabble")
              || arg.equalsIgnoreCase("-scrabble")) {
        Some(Flag.Scrabble)
      } else None
    }

    if (!args.isEmpty && !args.tail.isEmpty) {
      Some(args.toVector.tail.flatMap(flagFromString))
    } else None
  }

  def main(args: Array[String]): Unit = {
    val letters = parseLetters(args)
    val flags = parseFlags(args).flatten

    if (flags.contains(Flag.Scrabble)) {
      Sorter.sortScrabble(Finder.matchingWords(letters)).foreach(println)
    } else {
      Sorter.sortLenAlpha(Finder.matchingWords(letters)).foreach(println)
    }
  }
}
