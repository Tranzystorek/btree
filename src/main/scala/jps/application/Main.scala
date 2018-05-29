package jps.application

import jps.btree.BTree
import jps.btree.CustomStrictOrderings._

import scala.annotation.tailrec
import scala.io.StdIn

object Main extends App {
  private var btree = BTree.empty[Int]()

  private val numberRegex = "0|-?[1-9]\\d*"
  private val addCmd = raw"""\s*add\s*($numberRegex)\s*""".r
  private val rmCmd = raw"""\s*rm\s*($numberRegex)\s*""".r
  private val existsCmd = raw"""\s*is\s*($numberRegex)\s*""".r
  private val printCmd = """\s*print\s*""".r
  private val helpCmd = """\s*help\s*""".r
  private val exitCmd = """\s*exit\s*""".r

  private def printHelp: Unit = {
    println("Commands:\n" +
      "add [Int] - add value to BTree\n" +
      "rm [Int] - remove value from BTree\n" +
      "is [Int] - check if a value exists in BTree\n" +
      "print - print a BST view of BTree\n" +
      "help - print this help prompt\n" +
      "exit - exit program")
  }

  @tailrec
  final def mainLoop: Unit = {
    print("> ")

    StdIn.readLine match {
      case addCmd(insertedVal) => {
        val insertedInt = insertedVal.toInt

        if (!btree.contains(insertedInt)) {
          println("Adding " + insertedVal)
          btree = btree.insert(insertedInt)
        }
        else
          println("Value " + insertedVal + " already exists")
        mainLoop
      }
      case rmCmd(removedVal) => {
        val removedInt = removedVal.toInt

        if (btree.contains(removedInt)) {
          println("Removing " + removedVal)
          btree = btree.remove(removedInt)
        }
        else
          println("Value " + removedVal + " does not exist")
        mainLoop
      }
      case existsCmd(searchedVal) => {
        println(btree.contains(searchedVal.toInt))
        mainLoop
      }
      case printCmd(_*) => {
        println("Printing...")
        println(btree.toString)
        mainLoop
      }
      case helpCmd(_*) => {
        printHelp
        mainLoop
      }
      case exitCmd(_*) => println("Exiting...")
      case _ => println("Incorrect input"); mainLoop
    }
  }

  //main
  printHelp
  mainLoop
}
