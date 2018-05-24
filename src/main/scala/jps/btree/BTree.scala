package jps.btree

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, Queue}

/**
  *
  * @param degree
  */
case class Parameters(degree: Int = 5)

/**
  *
  * @param parameters
  * @tparam T type of values stored in the tree
  */
class BTree[T: StrictOrdering](parameters: Parameters = Parameters()) {

  private val minChildrenNumber: Int = parameters.degree
  private val maxChildrenNumber: Int = parameters.degree * 2

  /**
    *
    * @param node
    * @param value
    */
  case class Data(node: Option[Node], var value: T)

  private var root = new Node()

  /**
    * Inserts a value into the tree (only if it does not exist yet).
    *
    * @param insertedValue
    */
  def add(insertedValue: T): Unit = {
    root.get(insertedValue) match {
      case (parentNode, index, false) => parentNode.insertInLeaf(index, insertedValue)
      case _ => ()
    }
  }

  /**
    * Returns true if the tree contains given value, false otherwise.
    *
    * @param searchedValue
    * @return
    */
  def contains(searchedValue: T): Boolean = {
    root.get(searchedValue) match {
      case (_, _, retVal) => retVal
    }
  }

  /**
    * Removes given value from the tree (only if it already exists).
    *
    * @param removedValue
    */
  def remove(removedValue: T): Unit = {
    root.get(removedValue) match {
      case (parentNode, index, true) => parentNode.removeFromHere(index)
      case _ => ()
    }
  }

  /**
    * Prints out tree contents in a BFS order (DEBUG).
    */
  def printLevels(): Unit = {
    val queue = new Queue[(Node, Int)]
    queue.enqueue((root, 0))

    var currentParent = 0

    while (queue.nonEmpty) {
      val (node, parentNumber) = queue.dequeue()

      if (currentParent < parentNumber) {
        currentParent = parentNumber
        print("\n")
      }
      else
        print("| ")

      for (el <- node.children) {
        print(el.value + " ")

        el match {
          case Data(Some(nextNode), _) => queue.enqueue((nextNode, parentNumber + 1))
          case _ => ()
        }
      }

      if (node.lastChild.isDefined) {
        queue.enqueue((node.lastChild.get, parentNumber + 1))
      }
    }

    print("\n")
  }

  /**
    * Represents a single node in the BTree.
    * Parent is optional because BTree's root doesn't have one.
    *
    * @param children  ArrayBuffer of separators with nodes belonging before them
    * @param lastChild the child node that belongs after the last separator value
    * @param parent
    */
  class Node(var children: ArrayBuffer[Data] = ArrayBuffer[Data](),
             var lastChild: Option[Node] = None,
             var parent: Option[Node] = None) {
    /**
      * Remove value at the sepcified position from a node
      * by either removing it from leaf,
      * or moving the leftmost value from the right neighboring tree to it.
      *
      * Issues rebalance at an appropriate node.
      *
      * @param pos position which shall be removed
      */
    def removeFromHere(pos: Int): Unit = {
      if (lastChild.isEmpty) {
        children.remove(pos)
        rebalance()
      }
      else {
        val removedEntry = children(pos)

        //set head of the node to the right as new separator
        if (pos == children.size - 1) {
          val nextNode = lastChild.get.getLeftmostNode

          children.update(pos, Data(removedEntry.node, nextNode.children.head.value))
          nextNode.children = nextNode.children.tail

          nextNode.rebalance()
        }
        else {
          val nextNode = children(pos + 1).node.get.getLeftmostNode

          children.update(pos, Data(removedEntry.node, nextNode.children.head.value))
          nextNode.children = nextNode.children.tail

          nextNode.rebalance()
        }
      }
    } //removeFromHere

    @tailrec
    private def getLeftmostNode: Node = {
      children.head.node match {
        case Some(nextNode) => nextNode.getLeftmostNode
        case None => this
      }
    } //getLeftmostNode

    private def getLeftSiblingWithSep: Option[(Node, Int)] = {
      val comparator = implicitly[StrictOrdering[T]]

      parent match {
        case None => None
        case Some(parentNode) => {
          val leftVal = children.head.value
          val separatorIndex = parentNode.children.indexWhere(el => comparator.lessThan(leftVal, el.value))

          separatorIndex match {
            case 0 => None
            case -1 => Some(parentNode.children.last.node.get, parentNode.children.size - 1)
            case _ => Some(parentNode.children(separatorIndex - 1).node.get, separatorIndex - 1)
          }
        }
      }
    } //getLeftSiblingWithSep

    private def getRightSiblingWithSep: Option[(Node, Int)] = {
      val comparator = implicitly[StrictOrdering[T]]

      parent match {
        case None => None
        case Some(parentNode) => {
          val leftVal = children.head.value
          val separatorIndex = parentNode.children.indexWhere(el => comparator.lessThan(leftVal, el.value))

          separatorIndex match {
            case sepVal if sepVal == parentNode.children.size - 1 => Some(parentNode.lastChild.get, separatorIndex)
            case -1 => None
            case _ => Some(parentNode.children(separatorIndex + 1).node.get, separatorIndex)
          }
        }
      }
    } //getRightSiblingWithSep

    private def isDeficient: Boolean = {
      parent match {
        case None => false
        case Some(_) => children.size < minChildrenNumber
      }
    } //isDeficient

    @tailrec
    private def rebalance(): Unit = {
      if (isDeficient) {
        val parentNode = parent.get
        val rightSiblingWithSep = getRightSiblingWithSep
        lazy val leftSiblingWithSep = getLeftSiblingWithSep

        (rightSiblingWithSep, leftSiblingWithSep) match {
          case (Some((sibling, separatorValue)), _) if sibling.children.size > minChildrenNumber => {
            val separator = parentNode.children(separatorValue)
            children += Data(lastChild, separator.value)

            lastChild = sibling.children.head.node

            if (lastChild.isDefined)
              lastChild.get.parent = Some(this)

            separator.value = sibling.children.head.value
            sibling.children.remove(0)
          }
          case (_, Some((sibling, separatorValue))) if sibling.children.size > minChildrenNumber => {
            val separator = parentNode.children(separatorValue)
            Data(sibling.lastChild, separator.value) +=: children

            if (children.head.node.isDefined)
              children.head.node.get.parent = Some(this)

            sibling.lastChild = sibling.children.last.node
            separator.value = sibling.children.last.value
            sibling.children.remove(sibling.children.size - 1)
          }
          case (Some((sibling, separatorValue)), _) => {
            val separator = parentNode.children(separatorValue)

            children += Data(lastChild, separator.value)
            children ++= sibling.children
            lastChild = sibling.lastChild

            if (sibling.lastChild.isDefined) {
              for (Data(Some(childNode), _) <- sibling.children) {
                childNode.parent = Some(this)
              }

              sibling.lastChild.get.parent = Some(this)
            }

            if (separatorValue == parentNode.children.size - 1) {
              parentNode.lastChild = Some(this)
            }
            else {
              parentNode.children.update(separatorValue + 1,
                Data(Some(this), parentNode.children(separatorValue + 1).value))
            }

            parentNode.children.remove(separatorValue)

            if (parentNode.parent.isEmpty && parentNode.children.isEmpty) {
              lastChild = None
              parent = None
              root = this
            }
            else
              parentNode.rebalance()
          }
          case (_, Some((sibling, separatorValue))) => {
            val separator = parentNode.children(separatorValue)

            val newChildren = sibling.children :+ Data(separator.node.get.lastChild, separator.value)
            children = newChildren ++ children

            for (Data(Some(childNode), _) <- sibling.children) {
              childNode.parent = Some(this)
            }

            parentNode.children.remove(separatorValue)

            if (parentNode.parent.isEmpty && parentNode.children.isEmpty) {
              lastChild = None
              parent = None
              root = this
            }
            else
              parentNode.rebalance()
          }
        }
      }
    } //rebalance

    /**
      * Gets a parent node where the searched value belongs,
      * along with the appropriate position and information
      * whether the value already exists.
      *
      * @param searchedValue
      * @return ( parent node, position in its array, indication whether the value already exists )
      */
    @tailrec
    final def get(searchedValue: T): (Node, Int, Boolean) = {
      val comparator = implicitly[StrictOrdering[T]]

      children.zipWithIndex.find(el =>
        comparator.equal(searchedValue, el._1.value)
          || comparator.lessThan(searchedValue, el._1.value)) match {
        case Some((elem, index)) if comparator.equal(searchedValue, elem.value) => (this, index, true)
        case Some((elem, index)) => {
          elem.node match {
            case None => (this, index, false)
            case Some(nextNode) => nextNode.get(searchedValue)
          }
        }
        case None =>
          lastChild match {
            case None => (this, children.size, false)
            case Some(nextNode) => nextNode.get(searchedValue)
          }
      }
    } //get

    /**
      * Inserts given value at the specified position.
      *
      * Performs node split if needed.
      *
      * @param pos
      * @param insertedValue
      */
    def insertInLeaf(pos: Int, insertedValue: T): Unit = {
      children.insert(pos, Data(None, insertedValue))

      if (children.size > maxChildrenNumber) {
        splitToParent
      }
    } //insertInLeaf

    @tailrec
    private def splitToParent: Unit = {
      val halfSize = children.size / 2
      val middleSplit = (children.take(halfSize), children(halfSize), children.drop(halfSize + 1))

      parent match {
        //split Non-Root
        case Some(parentNode) => {
          val newNode = new Node(middleSplit._1, middleSplit._2.node, parent)

          children = middleSplit._3

          //switch children
          if (lastChild.isDefined) {
            for (Data(Some(childNode), _) <- newNode.children) {
              childNode.parent = Some(newNode)
            }

            newNode.lastChild.get.parent = Some(newNode)
          }

          parentNode.addToNode(Data(Some(newNode), middleSplit._2.value))

          if (parentNode.children.size > maxChildrenNumber)
            parentNode.splitToParent
        }

        //split Root
        case None => {
          val leftNode = new Node(middleSplit._1, middleSplit._2.node)
          val rightNode = new Node(middleSplit._3, lastChild)

          //switch parents
          if(lastChild.isDefined) {
            for (Data(Some(childNode), _) <- leftNode.children) {
              childNode.parent = Some(leftNode)
            }

            leftNode.lastChild.get.parent = Some(leftNode)

            for (Data(Some(childNode), _) <- rightNode.children) {
              childNode.parent = Some(rightNode)
            }

            rightNode.lastChild.get.parent = Some(rightNode)
          }

          val newRoot = new Node(ArrayBuffer[Data](Data(Some(leftNode), middleSplit._2.value)), Some(rightNode))

          leftNode.parent = Some(newRoot)
          rightNode.parent = Some(newRoot)

          root = newRoot
        }
      }
    } //splitToParent

    private def addToNode(valData: Data): Unit = {
      val insertIndex = children.indexWhere(el => implicitly[StrictOrdering[T]].lessThan(valData.value, el.value))

      if (insertIndex == -1)
        children += valData
      else
        children.insert(insertIndex, valData)
    } //addToLeaf

  }

}


