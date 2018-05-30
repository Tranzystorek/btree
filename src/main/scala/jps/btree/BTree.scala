package jps.btree

import scala.annotation.tailrec

object BTree {

  /**
    * Represents a single BTree node.
    *
    * @param data value entries
    * @param children child nodes
    * @tparam T type contained in the btree
    */
  case class Node[T: StrictOrdering](data: Vector[T], children: Vector[Node[T]]) {
    def isLeaf(): Boolean = children.isEmpty

    def childIndex(value: T): Int = {
      data.lastIndexWhere(implicitly[StrictOrdering[T]].lessThan(_, value)) + 1
    }
  }

  /**
    * Constructs an empty BTree with the specified degree.
    *
    * @param degree minimal number of data entries in a single node,
    *               maximum number is twice as much
    * @tparam T type contained in the BTree
    * @return a newly constructed empty BTree
    */
  def empty[T: StrictOrdering](degree: Int = 5): BTree[T] = BTree(Node[T](Vector.empty, Vector.empty), degree)
}

/**
  * Represents a whole BTree.
  *
  */
case class BTree[T: StrictOrdering](private val root: BTree.Node[T], private val degree: Int) {
  private val minDataNumber: Int = degree
  private val maxDataNumber: Int = 2 * degree

  private type N = BTree.Node[T]

  /**
    * Inserts a value in the BTree (only if it does not exist yet).
    *
    * @param newValue value to insert
    * @return a copy of this BTree with the element inserted
    */
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

  /**
    * Remove a value from the BTree (only if it already exists).
    *
    * @param oldValue value to be removed
    * @return a copy of this BTree with the element removed
    */
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

    def removeLeftmost(node: N): (T, N) = {
      if (node.isLeaf) {
        val removedValue = node.data.head

        (
          removedValue,
          node.copy(
            data = node.data.drop(1)
          )
        )
      }
      else {
        val (removedValue, modifiedChild) = removeLeftmost(node.children.head)

        if (modifiedChild.data.length < minDataNumber) {
          val separatorValue = node.data.head
          val rightSibling = node.children(1)

          if (rightSibling.data.length > minDataNumber)
            (removedValue, rotateLeft(0, node))
          else {
            val merged = mergeNodes(separatorValue, modifiedChild, rightSibling)

            (
              removedValue,
              node.copy(
                data = node.data.drop(1),
                children = node.children.drop(1).updated(0, merged)
              )
            )
          }
        }
        else
          (
            removedValue,
            node.copy(
              children = node.children.updated(0, modifiedChild)
            )
          )
      }
    }//removeLeftmost

    def removeRightmost(node: N): (T, N) = {
      if (node.isLeaf) {
        val removedValue = node.data.last

        (
          removedValue,
          node.copy(
            data = node.data.dropRight(1)
          )
        )
      }
      else {
        val (removedValue, modifiedChild) = removeRightmost(node.children.last)

        if (modifiedChild.data.length < minDataNumber) {
          val separatorValue = node.data.last
          val leftSiblingIndex = node.children.length - 2
          val leftSibling = node.children(leftSiblingIndex)

          if (leftSibling.data.length > minDataNumber) {
            (removedValue, rotateRight(node.data.length - 1, node))
          }
          else {
            val merged = mergeNodes(separatorValue, leftSibling, modifiedChild)

            (
              removedValue,
              node.copy(
                data = node.data.dropRight(1),
                children = node.children.dropRight(1).updated(leftSiblingIndex, merged)
              )
            )
          }
        }
        else
          (
            removedValue,
            node.copy(
              children = node.children.updated(node.children.length - 1, modifiedChild)
            )
          )
      }
    }//removeRightmost

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

          val childDataIndex = child.data.indexWhere(comparator.equal(oldValue, _))

          if (childDataIndex == -1) {
            //oldValue is not in child, go deeper
            val modifiedChild = tryRemove(child)
            val newNode = node.copy(
              children = node.children.updated(childIndex, modifiedChild)
            )

            if (modifiedChild.data.length < minDataNumber) {
              if (childIndex < newNode.children.length - 1) {
                val rightSiblingIndex = childIndex + 1
                val rightSibling = newNode.children(rightSiblingIndex)

                if (rightSibling.data.length > minDataNumber)
                  //rotate from right sibling
                  rotateLeft(childIndex, newNode)
                else {
                  val mergeSeparatorValue = newNode.data(childIndex)
                  val merged = mergeNodes(mergeSeparatorValue, modifiedChild, rightSibling)

                  newNode.copy(
                    data = newNode.data.take(childIndex) ++ newNode.data.drop(childIndex + 1),
                    children = (newNode.children.take(childIndex) :+ merged) ++ newNode.children.drop(childIndex + 2)
                  )
                }
              }
              else {
                val leftSiblingIndex = childIndex - 1
                val leftSibling = newNode.children(leftSiblingIndex)

                if (leftSibling.data.length > minDataNumber)
                  //rotate from left sibling
                  rotateRight(leftSiblingIndex, newNode)
                else {
                  val mergeSeparatorValue = newNode.data(leftSiblingIndex)
                  val merged = mergeNodes(mergeSeparatorValue, leftSibling, modifiedChild)

                  newNode.copy(
                    data = newNode.data.take(leftSiblingIndex) ++ newNode.data.drop(leftSiblingIndex + 1),
                    children = (newNode.children.take(leftSiblingIndex) :+ merged) ++ newNode.children.drop(leftSiblingIndex + 2)
                  )
                }
              }
            }
            else
              node.copy(
                children = node.children.updated(childIndex, modifiedChild)
              )
          }
          else {
            //oldValue is in child
            if (child.isLeaf && child.data.length > minDataNumber) {
              val newChild = child.copy(
                data = child.data.take(childDataIndex) ++ child.data.drop(childDataIndex + 1)
              )

              node.copy(
                children = node.children.updated(childIndex, newChild)
              )
            }
            else if (childIndex < node.children.length - 1) {
              //remove leftmost from the right sibling
              val rightSiblingIndex = childIndex + 1
              val (newSeparatorValue, modifiedRightSibling) = removeLeftmost(node.children(rightSiblingIndex))

              val oldSeparatorValue = node.data(childIndex)
              val childWithValueRemoved = child.copy(
                data = child.data.take(childDataIndex) ++ child.data.drop(childDataIndex + 1) :+ oldSeparatorValue
              )
              val newNode = node.copy(
                data = node.data.updated(childIndex, newSeparatorValue)
              )

              if (modifiedRightSibling.data.length < minDataNumber) {
                //rotate/merge
                if (rightSiblingIndex < node.children.length - 1) {
                  val nextSiblingIndex = rightSiblingIndex + 1
                  val nextSibling = node.children(nextSiblingIndex)
                  val newerNode = newNode.copy(
                    children = newNode.children.updated(childIndex, childWithValueRemoved)
                      .updated(rightSiblingIndex, modifiedRightSibling)
                  )

                  if (nextSibling.data.length > minDataNumber)
                    //rotate from the next right sibling
                    rotateLeft(rightSiblingIndex, newerNode)
                  else {
                    //merge with the next right sibling
                    val mergeSeparatorValue = node.data(rightSiblingIndex)
                    val merged = mergeNodes(mergeSeparatorValue, modifiedRightSibling, nextSibling)

                    newerNode.copy(
                      data = newerNode.data.take(rightSiblingIndex) ++ newerNode.data.drop(rightSiblingIndex + 1),
                      children = (newerNode.children.take(rightSiblingIndex) :+ merged) ++ newerNode.children.drop(rightSiblingIndex + 2)
                    )
                  }
                }
                else {
                  if (childWithValueRemoved.data.length > minDataNumber)
                    rotateRight(childIndex, newNode)
                  else {
                    val merged = mergeNodes(newSeparatorValue, childWithValueRemoved, modifiedRightSibling)

                    newNode.copy(
                      data = newNode.data.take(childIndex) ++ newNode.data.drop(childIndex + 1),
                      children = (newNode.children.take(childIndex) :+ merged) ++ newNode.children.drop(childIndex + 2)
                    )
                  }
                }
              }
              else
                newNode.copy(
                  children = node.children.updated(childIndex, childWithValueRemoved)
                    .updated(rightSiblingIndex, modifiedRightSibling)
                )
            }
            else {
              //remove rightmost from the left sibling
              val leftSiblingIndex = childIndex - 1
              val (newSeparatorValue, modifiedLeftSibling) = removeRightmost(node.children(leftSiblingIndex))

              val oldSeparatorValue = node.data(leftSiblingIndex)
              val childWithValueRemoved = child.copy(
                data = oldSeparatorValue +: (child.data.take(childDataIndex) ++ child.data.drop(childDataIndex + 1))
              )
              val newNode = node.copy(
                data = node.data.updated(leftSiblingIndex, newSeparatorValue)
              )

              if (modifiedLeftSibling.data.length < minDataNumber) {
                //rotate/merge
                if (leftSiblingIndex > 0) {
                  val previousSiblingIndex = leftSiblingIndex - 1
                  val previousSibling = node.children(previousSiblingIndex)
                  val newerNode = newNode.copy(
                    children = newNode.children.updated(childIndex, childWithValueRemoved)
                      .updated(leftSiblingIndex, modifiedLeftSibling)
                  )

                  if (previousSibling.data.length > minDataNumber)
                    //rotate from the previous left sibling
                    rotateRight(previousSiblingIndex, newerNode)
                  else {
                    //merge with the previous left sibling
                    val mergeSeparatorValue = node.data(previousSiblingIndex)
                    val merged = mergeNodes(mergeSeparatorValue, previousSibling, modifiedLeftSibling)

                    newerNode.copy(
                      data = newerNode.data.take(previousSiblingIndex) ++ newerNode.data.drop(previousSiblingIndex + 1),
                      children = (newerNode.children.take(previousSiblingIndex) :+ merged) ++ newerNode.children.drop(previousSiblingIndex + 2)
                    )
                  }
                }
                else {
                  if (childWithValueRemoved.data.length > minDataNumber)
                    rotateLeft(leftSiblingIndex, newNode)
                  else {
                    val merged = mergeNodes(newSeparatorValue, modifiedLeftSibling, childWithValueRemoved)

                    newNode.copy(
                      data = newNode.data.take(leftSiblingIndex) ++ newNode.data.drop(leftSiblingIndex + 1),
                      children = (newNode.children.take(leftSiblingIndex) :+ merged) ++ newNode.children.drop(leftSiblingIndex + 2)
                    )
                  }
                }
              }
              else
                newNode.copy(
                  children = newNode.children.updated(leftSiblingIndex, modifiedLeftSibling)
                    .updated(childIndex, childWithValueRemoved)
                )
            }
          }
        }
      }

      //remove the value from here
      else {
        if(node.isLeaf)
          node.copy(
            data = node.data.take(dataIndex) ++ node.data.drop(dataIndex + 1)
          )
        else {
          val rightIndex = dataIndex + 1
          val rightNode = node.children(rightIndex)

          val (newValue, modifiedRight) = removeLeftmost(rightNode)

          val newNode = node.copy(
            data = node.data.updated(dataIndex, newValue)
          )

          if (modifiedRight.data.length < minDataNumber) {
            if (rightIndex < node.children.length - 1) {
              val nextRightIndex = rightIndex + 1
              val nextRight = node.children(nextRightIndex)

              val newerNode = newNode.copy(
                children = newNode.children.updated(rightIndex, modifiedRight)
              )

              if (nextRight.data.length > minDataNumber)
                //rotate from the next right node
                rotateLeft(rightIndex, newerNode)
              else {
                //merge with next right Node
                val mergeSeparatorValue = newerNode.data(rightIndex)
                val merged = mergeNodes(mergeSeparatorValue, modifiedRight, nextRight)

                newerNode.copy(
                  data = newerNode.data.take(rightIndex) ++ newerNode.data.drop(rightIndex + 1),
                  children = (newerNode.children.take(rightIndex) :+ merged) ++ newerNode.children.drop(rightIndex + 2)
                )
              }
            }
            else {
              val leftNode = newNode.children(dataIndex)

              val newerNode = newNode.copy(
                children = newNode.children.updated(rightIndex, modifiedRight)
              )

              if (leftNode.data.length > minDataNumber) {
                //rotate from left node
                rotateRight(dataIndex, newerNode)
              }
              else {
                //merge with left node
                val merged = mergeNodes(newValue, leftNode, modifiedRight)

                newerNode.copy(
                  data = newerNode.data.take(dataIndex) ++ newerNode.data.drop(dataIndex + 1),
                  children = (newerNode.children.take(dataIndex) :+ merged) ++ newerNode.children.drop(dataIndex + 2)
                )
              }
            }
          }
          else
            newNode.copy(
              children = node.children.updated(rightIndex, modifiedRight)
            )
        }
      }
    }//tryRemove

    this.copy(
      root = tryRemove(root)
    )
  }//remove

  /**
    * Check whether an element exists in the BTree.
    *
    * @param searchedValue value to be searched
    * @return true if the value exists in the BTree, false otherwise
    */
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

  /**
    * Present the BTree in human-readable form (DFS view).
    *
    * @return String that represents the BTree's contents
    */
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