package jps.btree

import scala.annotation.tailrec

object BTree {
  case class Node[T: StrictOrdering](data: Vector[T], children: Vector[Node[T]]) {
    def isLeaf(): Boolean = children.isEmpty

    def childIndex(value: T): Int = {
      data.lastIndexWhere(implicitly[StrictOrdering[T]].lessThan(_, value)) + 1
    }
  }

  def empty[T: StrictOrdering](degree: Int = 5): BTree[T] = BTree(Node[T](Vector.empty, Vector.empty), degree)
}

case class BTree[T: StrictOrdering](root: BTree.Node[T], degree: Int) {
  val minDataNumber: Int = degree
  val maxDataNumber: Int = 2 * degree

  private type N = BTree.Node[T]

  def insert(newValue: T): BTree[T] = {

    val comparator = implicitly[StrictOrdering[T]]

    def splitNode(node: N): (T, N, N) = (
      node.data(minDataNumber),
      BTree.Node(
        data = node.data.take(minDataNumber),
        children = node.children.take(minDataNumber + 1)
      ),
      BTree.Node(
        data = node.data.takeRight(minDataNumber),
        children = node.children.takeRight(minDataNumber + 1)
      )
    )

    def tryInsert(node: N): Either[(T, N, N), N] = {

      val dataIndex = node.data.indexWhere(comparator.equal(newValue, _))

      //Value already exists here
      if (dataIndex > -1)
        Right(node.copy())

      //try to insert here or in child
      else {
        val modified =
          if (node.isLeaf) {
            val newData = (node.data :+ newValue).sortWith((a, b) => comparator.lessThan(a, b))
            node.copy(data = newData)
          }
          else {
            val childIndex = node.childIndex(newValue)
            val child = node.children(childIndex)
            tryInsert(child) match {
              case Left((separatorValue, leftSplit, rightSplit)) =>
                val (left, right) = node.children.splitAt(childIndex)
                node.copy(
                  data = (node.data :+ separatorValue).sortWith((a, b) => comparator.lessThan(a, b)),
                  children = (left :+ leftSplit :+ rightSplit) ++ right.tail
                )
              case Right(modifiedChild) =>
                node.copy(children = node.children.updated(childIndex, modifiedChild))
            }
          }

        if (modified.data.length == maxDataNumber + 1) Left(splitNode(modified))
        else Right(modified)
      }
    }//tryInsert

    this.copy(
      root = tryInsert(root) match {
        case Right(node) => node
        case Left((separatorValue, leftSplit, rightSplit)) =>
          BTree.Node(Vector(separatorValue), Vector(leftSplit, rightSplit))
      }
    )
  }

  def remove(oldValue: T): BTree[T] = {

    val comparator = implicitly[StrictOrdering[T]]

    def mergeNodes(separatorValue: T, left: N, right: N) =
      BTree.Node(
        data = (left.data :+ separatorValue) ++ right.data,
        children = left.children ++ right.children
      )

    def rotateLeft(separatorIndex: Int, parent: N): N = {
      val rightNode = parent.children(separatorIndex + 1)
      val leftNode = parent.children(separatorIndex)
      val separatorValue = parent.data(separatorIndex)
      val newSeparator = rightNode.data.head

      val newLeft = leftNode.copy(
        data = leftNode.data :+ separatorValue,
        children =
          if (leftNode.children.nonEmpty)
            leftNode.children :+ rightNode.children.head
          else
            leftNode.children
      )

      val newRight = rightNode.copy(
        data = rightNode.data.drop(1),
        children =
          if (rightNode.children.nonEmpty)
            rightNode.children.drop(1)
          else
            rightNode.children
      )

      parent.copy(
        data = parent.data.updated(separatorIndex, newSeparator),
        children = parent.children.updated(separatorIndex, newLeft)
          .updated(separatorIndex + 1, newRight)
      )
    }//rotateLeft

    def rotateRight(separatorIndex: Int, parent: N): N = {
      val leftNode = parent.children(separatorIndex)
      val rightNode = parent.children(separatorIndex + 1)
      val separatorValue = parent.data(separatorIndex)
      val newSeparator = leftNode.data.last

      val newRight = rightNode.copy(
        data = separatorValue +: rightNode.data,
        children =
          if (rightNode.children.nonEmpty)
            leftNode.children.last +: rightNode.children
          else
            rightNode.children
      )

      val newLeft = leftNode.copy(
        data = leftNode.data.dropRight(1),
        children =
          if (leftNode.children.nonEmpty)
            leftNode.children.dropRight(1)
          else
            leftNode.children
      )

      parent.copy(
        data = parent.data.updated(separatorIndex, newSeparator),
        children = parent.children.updated(separatorIndex, newLeft)
          .updated(separatorIndex + 1, newRight)
      )
    }//rotateRight

    def tryRemove(node: N): N = {

      val dataIndex = node.data.indexWhere(comparator.equal(oldValue, _))

      //removed value is not here
      if (dataIndex == -1) {
        if(node.isLeaf)
          //value does not exist, rebuild tree
          node.copy()
        else {
          val childIndex = node.childIndex(oldValue)
          val child = node.children(childIndex)
          val modifiedChild = tryRemove(child)

          val newNode = node.copy(
            children = node.children.updated(childIndex, modifiedChild)
          )

          if(modifiedChild.children.length < minDataNumber) {
            lazy val rightSibling = newNode.children(childIndex + 1)
            lazy val leftSibling = newNode.children(childIndex - 1)

            val hasRight = childIndex < newNode.children.length - 1
            val hasLeft = childIndex > 0

            if (hasRight && rightSibling.data.length > minDataNumber) {
              //right sibling exists and has a surplus of children
              rotateLeft(childIndex, newNode)
            }
            else if (hasLeft && leftSibling.data.length > minDataNumber) {
              //left sibling exists and has a surplus of children
              rotateRight(childIndex - 1, newNode)
            }
            //merge necessary
            else if (hasRight) {
              val separatorValue = newNode.data(childIndex)

              newNode.copy(
                data = newNode.data.take(childIndex) ++ newNode.data.drop(childIndex + 1),
                children = (newNode.children.take(childIndex)
                  :+ mergeNodes(separatorValue, newNode.children(childIndex), rightSibling))
                  ++ newNode.children.drop(childIndex + 2)
              )
            }
            else {
              val separatorValue = newNode.data(childIndex - 1)

              newNode.copy(
                data = newNode.data.take(childIndex - 1) ++ newNode.data.drop(childIndex),
                children = (newNode.children.take(childIndex - 1)
                  :+ mergeNodes(separatorValue, leftSibling, newNode.children(childIndex)))
                  ++ newNode.children.drop(childIndex + 1)
              )
            }
          }
          else
            newNode
        }
      }

      //remove the value from here
      else {
        if(node.isLeaf)
          node.copy(
            data = node.data.take(dataIndex) ++ node.data.drop(dataIndex + 1)
          )
        else {
          val rightNode = node.children(dataIndex + 1)
          lazy val leftNode = node.children(dataIndex)

          if (rightNode.children.length > minDataNumber)
            rotateLeft(dataIndex, node)
          else if (leftNode.children.length > minDataNumber)
            rotateRight(dataIndex, node)
          else {
            val mergedChildren = mergeNodes(oldValue, leftNode, rightNode)
            val mergedChildrenWithoutSep = mergedChildren.copy(
              data = mergedChildren.data.take(minDataNumber) ++ mergedChildren.data.drop(minDataNumber + 1),
              children = mergedChildren.children.take(minDataNumber) ++ mergedChildren.children.drop(minDataNumber + 1)
            )

            if (node.data.length > 1)
              node.copy(
                data = node.data.take(dataIndex) ++ node.data.drop(dataIndex + 1),
                children = (node.children.take(dataIndex) :+ mergedChildrenWithoutSep) ++ node.children.drop(dataIndex + 2)
              )
            else {
              //destroy root
              mergedChildrenWithoutSep
            }
          }
        }
      }
    }//tryRemove

    this.copy(
      root = tryRemove(root)
    )
  }//remove

  def contains(searchedValue: T): Boolean = {

    val comparator = implicitly[StrictOrdering[T]]

    @tailrec
    def aux(node: N): Option[T] = {
      val valOpt = node.data.find(comparator.equal(_, searchedValue))

      if(node.isLeaf) valOpt
      else valOpt match {
        case None => aux(node.children(node.childIndex(searchedValue)))
        case retVal => retVal
      }
    }

    aux(root) match {
      case Some(_) => true
      case None => false
    }
  }//contains

  override def toString: String = {
    def aux(node: N, partialString: String): String = {
      val retString = node.data.mkString(",")

      if(!node.isLeaf) {
        val childrenString = node.children.map(child => aux(child, partialString + " ")).mkString("\n")

        s"$partialString$retString\n$childrenString"
      }
      else
        s"$partialString$retString"
    }

    aux(root, "")
  }//toString
}