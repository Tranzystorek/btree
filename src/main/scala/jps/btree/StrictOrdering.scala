package jps.btree

/**
  * Defines strict ordering rules for type required to be able
  * to store values in a BTree
  *
  * @tparam T
  */
trait StrictOrdering[-T] {
  def lessThan(a: T, b: T): Boolean
  def equal(a: T, b: T): Boolean
}

object CustomStrictOrderings {

  implicit object IntStrictOrdering extends StrictOrdering[Int] {
    override def lessThan(a: Int, b: Int): Boolean = a < b
    override def equal(a: Int, b: Int): Boolean = a == b
  }

  implicit object StringStrictOrdering extends StrictOrdering[String] {
    override def lessThan(a: String, b: String): Boolean = a.compareTo(b) < 0
    override def equal(a: String, b: String): Boolean = a == b
  }
}
