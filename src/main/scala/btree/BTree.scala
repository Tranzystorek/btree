package btree

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

case class Parameters(degree: Int = 10)

class BTree[T: StrictOrdering](parameters: Parameters = Parameters()) {
  case class Parameters(degree: Int = 10)

  val maxChildrenNumber = parameters.degree * 2

  case class Data(node: Option[Node], value: T)

  private var root_ = new Node()

  def add(insertedValue: T) = {
    if(!contains(insertedValue)) {
      root_.add(insertedValue)
    }
  }

  def contains(searchedValue: T) = {
    root_.find(searchedValue) match {
      case Some(_) => true
      case None => false
    }
  }

  def printLevels() = {
    var queue = new Queue[Node]
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

      if(!node.lastChild.isEmpty) {
        queue.enqueue(node.lastChild.get)
      }

      print("\n")
    }
  }

  class Node(var children: ListBuffer[Data] = ListBuffer[Data](),
             val lastChild: Option[Node] = None,
             var parent: Option[Node] = None) {
    def add(insertedValue: T): Unit = {
      val insertNode = children.find {
        case Data(_, nodeValue) => implicitly[StrictOrdering[T]].lessThan(insertedValue, nodeValue)
      }.getOrElse(lastChild)

      insertNode match {
          //node falls in the middle of list
        case Some(Data(Some(nextNode), _)) => nextNode.add(insertedValue)
        case Some(Data(None, _)) => addToLeaf(Data(None, insertedValue))

          //node belongs at the end
        case Some(nextNode: Node) => nextNode.add(insertedValue)
        case None => addToLeaf(Data(None, insertedValue))
      }
    }//add

    def remove(removedValue: T) = {
      //throws if removedValue does not exist
      val (superNode, pos) = get(removedValue).get

      //TODO implement removal
    }

    //get pair of ( node containing value, position in its children list )
    def get(searchedValue: T): Option[(Node, Int)] = {
      val comparator = implicitly[StrictOrdering[T]]

      def searchFor(sVal: T): (Option[(Data, Int)], Boolean) = {
        for(el <- children.zipWithIndex) {
          if(comparator.equal(sVal, el._1.value)) return (Some(el), true)
          else if(comparator.lessThan(sVal, el._1.value)) return (Some(el), false)
        }

        (None, false)
      }

      searchFor(searchedValue) match {
        case ( Some((_, pos)), true ) => Some((this, pos))
        case ( Some((Data(Some(nextNode), _), _)), false ) => nextNode.get(searchedValue)
        case ( Some((Data(None, _), _)), false ) => None
        case ( None, _ ) => {
          lastChild match {
            case Some(nextNode) => nextNode.get(searchedValue)
            case None => None
          }
        }
      }
    }//get

    def find(searchedValue: T): Option[T] = {
      val comparator = implicitly[StrictOrdering[T]]

      def searchFor(sVal: T): (Option[Data], Boolean) = {
        for(el <- children) {
          if(comparator.equal(sVal, el.value)) return (Some(el), true)
          else if(comparator.lessThan(sVal, el.value)) return (Some(el), false)
        }

        (None, false)
      }

      searchFor(searchedValue: T) match {
        case ( Some(Data(_, retVal)), true ) => Some(retVal)
        case ( Some(Data(Some(nextNode), _)), false ) => nextNode.find(searchedValue)
        case ( Some(Data(None, _)), false ) => None
        case ( None, _ ) => {
          lastChild match {
            case Some(nextNode) => nextNode.find(searchedValue)
            case None => None
          }
        }
      }
    }//find

    private def addToLeaf(valData: Data): Unit = {
      val splitIndex = children.indexWhere(el => implicitly[StrictOrdering[T]].lessThan(valData.value, el.value))

      if(splitIndex == -1) {
        children += valData
      }
      else {
        val splitted = children.splitAt(splitIndex)

        children = splitted._1 ++: valData +: splitted._2
      }

      if(children.size > maxChildrenNumber) {
        val middleSplit = children.splitAt(children.size / 2)
        val midVal = middleSplit._2.head

        parent match {
            //add to Non-Root
          case Some(parentNode) => {
            children = middleSplit._2.tail
            val newNode = new Node(middleSplit._1, midVal.node, parent)

            parentNode.addToLeaf(Data(Some(newNode), midVal.value))
          }

            //add to Root
          case None => {
            val leftNode = new Node(middleSplit._1, midVal.node)
            val rightNode = new Node(middleSplit._2.tail, lastChild)
            val newRoot = new Node(ListBuffer[Data](Data(Some(leftNode), midVal.value)), Some(rightNode))

            leftNode.parent = Some(newRoot)
            rightNode.parent = Some(newRoot)

            root_ = newRoot
          }
        }
      }
    }//addToLeaf

  }
}


