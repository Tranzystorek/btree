package btree

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

  private var root_ = new Node()

  /**
    * Inserts a value into the tree (only if it does not exist yet).
    *
    * @param insertedValue
    */
  def add(insertedValue: T): Unit = {
    root_.get(insertedValue) match {
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
    root_.get(searchedValue) match {
      case (_, _, retVal) => retVal
    }
  }

  /**
    * Removes given value from the tree (only if it already exists).
    *
    * @param removedValue
    */
  def remove(removedValue: T): Unit = {
    root_.get(removedValue) match {
      case (parentNode, index, true) => parentNode.removeFromHere(index)
      case _ => ()
    }
  }

  /**
    * Prints out tree contents in a BFS order (DEBUG).
    */
  def printLevels(): Unit = {
    val queue = new Queue[Node]
    queue.enqueue(root_)

    while(queue.nonEmpty) {
      val node = queue.dequeue()

      for(el <- node.children) {
        print(el.value + " ")

        el match {
          case Data(Some(nextNode), _) => queue.enqueue(nextNode)
          case _ => ()
        }
      }

      if(node.lastChild.isDefined) {
        queue.enqueue(node.lastChild.get)
      }

      print("\n")
    }
  }

  /**
    *
    * @param children
    * @param lastChild
    * @param parent
    */
  class Node(var children: ArrayBuffer[Data] = ArrayBuffer[Data](),
             var lastChild: Option[Node] = None,
             var parent: Option[Node] = None) {
    def removeFromHere(pos: Int): Unit = {
      if(lastChild.isEmpty) {
        children.remove(pos)
        rebalance()
      }
      else {
        val removedEntry = children(pos)

        //set head of the node to the right as new separator
        if(pos == children.size - 1) {
          val nextNode = lastChild.get.getLeftMost

          children.update(pos, Data(removedEntry.node, nextNode.children.head.value))
          nextNode.children = nextNode.children.tail

          nextNode.rebalance()
        }
        else {
          val nextNode = children(pos + 1).node.get.getLeftMost

          children.update(pos, Data(removedEntry.node, nextNode.children.head.value))
          nextNode.children = nextNode.children.tail

          nextNode.rebalance()
        }
      }
    }//removeFromHere

    @scala.annotation.tailrec
    private def getLeftMost: Node = {
      children.head.node match {
        case Some(nextNode) => nextNode.getLeftMost
        case None => this
      }
    }//getLeftMost

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
            case _ => Some(parentNode.children(separatorIndex - 1).node.get, separatorIndex)
          }
        }
      }
    }//getLeftSibling

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
    }//getRightSibling

    private def isDeficient: Boolean = {
      parent match {
        case None => false
        case Some(_) => children.size < minChildrenNumber
      }
    }//isDeficient

    private def rebalance(): Unit = {
      if(isDeficient) {
        val parentNode = parent.get
        val rightSiblingWithSep = getRightSiblingWithSep

        //right sibling has enough children
        for((sibling, separatorValue) <- rightSiblingWithSep if sibling.children.size > minChildrenNumber) {
          val separator = parentNode.children(separatorValue)
          children += Data(lastChild, separator.value)

          lastChild = sibling.children.head.node
          separator.value = sibling.children.head.value
          sibling.children.remove(0)

          return
        }

        val leftSiblingWithSep = getLeftSiblingWithSep

        //left sibling has enough children
        for((sibling, separatorValue) <- leftSiblingWithSep if sibling.children.size > minChildrenNumber) {
          val separator = parentNode.children(separatorValue)
          Data(sibling.lastChild, separator.value) +=: children

          sibling.lastChild = sibling.children.last.node
          separator.value = sibling.children.last.value
          sibling.children.remove(sibling.children.size - 1)

          return
        }

        //merge this node with right sibling
        for((sibling, separatorValue) <- rightSiblingWithSep) {
          val separator = parentNode.children(separatorValue)

          children += Data(lastChild, separator.value)
          children ++= sibling.children
          lastChild = sibling.lastChild

          if(separatorValue == parentNode.children.size - 1) {
            parentNode.lastChild = Some(this)
          }
          else {
            parentNode.children.update(separatorValue + 1,
              Data(Some(this), parentNode.children(separatorValue + 1).value))
          }

          parentNode.children.remove(separatorValue)

          if(parentNode.parent.isEmpty && parentNode.children.isEmpty) {
            lastChild = None
            root_ = this
          }
          else
            parentNode.rebalance()

          return
        }

        //merge this node with left sibling
        for((sibling, separatorValue) <- leftSiblingWithSep) {
          val separator = parentNode.children(separatorValue)

          val newChildren = sibling.children :+ Data(separator.node.get.lastChild, separator.value)
          children = newChildren ++ children

          parentNode.children.remove(separatorValue)

          if(parentNode.parent.isEmpty && parentNode.children.isEmpty) {
            lastChild = None
            root_ = this
          }
          else
            parentNode.rebalance()
        }
      }
    }//rebalance

    def get(searchedValue: T): (Node, Int, Boolean) = {
      val comparator = implicitly[StrictOrdering[T]]

      for((elem, index) <- children.zipWithIndex) {
        if(comparator.equal(searchedValue, elem.value))
          return (this, index, true)
        else if(comparator.lessThan(searchedValue, elem.value))
          elem.node match {
            case None => return (this, index, false)
            case Some(nextNode) => return nextNode.get(searchedValue)
          }
      }

      lastChild match {
        case None => (this, children.size, false)
        case Some(nextNode) => nextNode.get(searchedValue)
      }
    }//get

    def insertInLeaf(pos: Int, insertedValue: T): Unit = {
      children.insert(pos, Data(None, insertedValue))

      if(children.size > maxChildrenNumber) {
        splitToParent
      }
    }//insertInLeaf

    private def splitToParent = {
      val halfSize = children.size / 2
      val middleSplit = (children.take(halfSize), children(halfSize), children.drop(halfSize + 1))

      parent match {
        //add to Non-Root
        case Some(parentNode) => {
          children = middleSplit._3
          val newNode = new Node(middleSplit._1, middleSplit._2.node, parent)

          parentNode.addToNode(Data(Some(newNode), middleSplit._2.value))
        }

        //add to Root
        case None => {
          val leftNode = new Node(middleSplit._1, middleSplit._2.node)
          val rightNode = new Node(middleSplit._3, lastChild)
          val newRoot = new Node(ArrayBuffer[Data](Data(Some(leftNode), middleSplit._2.value)), Some(rightNode))

          leftNode.parent = Some(newRoot)
          rightNode.parent = Some(newRoot)

          root_ = newRoot
        }
      }
    }//splitToParent

    private def addToNode(valData: Data): Unit = {
      val insertIndex = children.indexWhere(el => implicitly[StrictOrdering[T]].lessThan(valData.value, el.value))

      if(insertIndex == -1)
        children += valData
      else
        children.insert(insertIndex, valData)

      if(children.size > maxChildrenNumber)
        splitToParent
    }//addToLeaf

  }
}


