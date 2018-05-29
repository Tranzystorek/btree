import jps.btree.CustomStrictOrderings._
import jps.btree.BTree

class BTreeTest extends org.scalatest.FunSuite {
  test("when value is inserted, BTree contains it") {
    var btree = BTree.empty[Int]()
    btree = btree.insert(10)

    assert(btree.contains(10))
  }

  test("when value is removed, BTree no longer contains it") {
    var btree = BTree.empty[Int]()
    btree = btree.insert(10)
    assert(btree.contains(10))

    btree = btree.remove(10)
    assert(!btree.contains(10))
  }

  test("when there are many values, and one is removed, then Btree no longer contains it") {
    var btree = BTree.empty[Int]()

    for(i <- (1 to 11)) {
      btree = btree.insert(i)
    }

    assert(btree.contains(6))

    btree = btree.remove(6)
    assert(!btree.contains(6))
  }

  test("when many values are inserted, BTree contains them") {
    var btree = BTree.empty[Int]()

    for(i <- (1 to 11)) {
      btree = btree.insert(i)
    }

    assert(btree.contains(1))
    assert(btree.contains(2))
    assert(btree.contains(3))
    assert(btree.contains(4))
    assert(btree.contains(5))
    assert(btree.contains(6))
    assert(btree.contains(7))
    assert(btree.contains(8))
    assert(btree.contains(9))
    assert(btree.contains(10))
    assert(btree.contains(11))
  }

  test("BTree works with strings") {
    var btree = BTree.empty[String]()
    btree = btree.insert("Abcd")

    assert(btree.contains("Abcd"))
  }
}
