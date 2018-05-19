import btree.CustomStrictOrderings._
import btree.Parameters
import btree.BTree

class BTreeTest extends org.scalatest.FunSuite {
  test("when value is inserted, BTree contains it") {
    var btree = new BTree[Int]
    btree.add(10)

    assert(btree.contains(10))
  }

  test("when many values are inserted, BTree contains them") {
    var btree = new BTree[Int](Parameters(2))

    for(i <- (1 to 7)) {
      btree.add(i)
    }

    assert(btree.contains(1))
    assert(btree.contains(2))
    assert(btree.contains(3))
    assert(btree.contains(4))
    assert(btree.contains(5))
    assert(btree.contains(6))
    assert(btree.contains(7))
 }

  test("BTree works with strings") {
    var btree = new BTree[String]
    btree.add("Abcd")

    assert(btree.contains("Abcd"))
  }
}
