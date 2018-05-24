package jps.application

import jps.btree.BTree
import jps.btree.CustomStrictOrderings._

import scala.annotation.tailrec
import scala.io.StdIn

object Main extends App {
  private val btree = new BTree[Int]

  private val numberRegex = "0|-?[1-9]\\d*"
  private val addCmd = raw"""\s*add\s*($numberRegex)\s*""".r
  private val rmCmd = raw"""\s*rm\s*($numberRegex)\s*""".r
  private val existsCmd = raw"""\s*is\s*($numberRegex)\s*""".r
  private val printCmd = """\s*print\s*""".r
  private val exitCmd = """\s*exit\s*""".r

  @tailrec
  final def mainLoop: Unit = {
    val line = StdIn.readLine()

    print("> ")

    line match {
      case addCmd(insertedVal) => {
        val insertedInt = insertedVal.toInt

        if(!btree.contains(insertedInt)) {
          println("Adding " + insertedVal)
          btree.add(insertedInt)
        }
        else
          println("Value " + insertedVal + " already exists")
        mainLoop
      }
      case rmCmd(removedVal) => {
        val removedInt = removedVal.toInt

        if(btree.contains(removedInt)) {
          println("Removing " + removedVal)
          btree.remove(removedInt)
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
        btree.printLevels()
        mainLoop
      }
      case exitCmd(_*) => println("Exiting...")
      case _ => println("Incorrect input"); mainLoop
    }
  }

  //main
  println("Commands:\n" +
          "add [Int] - add value to BTree\n" +
          "rm [Int] - remove value from BTree\n" +
          "is [Int] - check if a value exists in BTree\n" +
          "print - print a BST view of BTree\n" +
          "exit - exit program")
  mainLoop
}
