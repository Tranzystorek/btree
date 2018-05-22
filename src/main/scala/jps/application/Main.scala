package jps.application

import jps.btree.BTree
import jps.btree.CustomStrictOrderings._
import scala.io.StdIn

object Main extends App {
  private val btree = new BTree[Int]

  private val addCmd = """add\s*(0|-?[1-9]\d*)\s*""".r
  private val rmCmd = """rm\s*(0|-?[1-9]\d*)\s*""".r
  private val existsCmd = """is\s*(0|-?[1-9]\d*)\s*""".r
  private val printCmd = """print\s*""".r
  private val exitCmd = """exit\s*""".r

  def mainLoop: Unit = {
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
