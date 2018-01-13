package scala

import scala.Main.Flag.Flag

object Main {

  object Flag extends Enumeration {
    type Flag = Value
    val Scrabble, Help = Value
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
      if (arg.equalsIgnoreCase("-s")
              || arg.equalsIgnoreCase("--scrabble")) {
        Some(Flag.Scrabble)
      } else if (arg.equalsIgnoreCase("-h")
              || arg.equalsIgnoreCase("--help")
              || arg.equalsIgnoreCase("help")) {
        Some(Flag.Help)
      } else None
    }

    if (!args.isEmpty) {
      Some(args.toVector.flatMap(flagFromString))
    } else None
  }

  def main(args: Array[String]): Unit = {
    val letters = parseLetters(args)
    val flags = parseFlags(args) match {
      case None => Vector()
      case Some(v: Vector[Flag]) => v
    }

    if (flags.contains(Flag.Help)) {
      println("Arguments usage: ")
      println("The first argument should be the letters you want to use, ")
      println("written as one word (for example, if you have 'a, b, d, x'")
      println("You write the argument as 'abdx')")
      println()
      println("Following your letters with -s or --scrabble will sort")
      println("the words by scrabble score.")
    } else if (flags.contains(Flag.Scrabble)) {
      Sorter.sortScrabble(Finder.matchingWords(letters)).foreach(println)
    } else {
      Sorter.sortLenAlpha(Finder.matchingWords(letters)).foreach(println)
    }
  }
}
