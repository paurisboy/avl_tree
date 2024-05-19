import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

trait Tree[+T] {
  val height: Int
  def rotateLeft: Tree[T]
  def rotateRight: Tree[T]
  def balanceFactor: Int
  def insert[A >: T : Ordering](elem: A): Tree[A]
  protected def balance: Tree[T]
  def print(prefix: String = "", isTail: Boolean = true): String
}

object Tree {
  case class Node[+T](left: Tree[T], right: Tree[T], data: T) extends Tree[T] {

    val height = 1 + Math.max(left.height, right.height)

    def rotateLeft: Tree[T] = right match {
      case Node(rl, rr, rdata) =>
        val newLeft = Node(left, rl, data)
        Node(newLeft, rr, rdata)
      case Empty => this
    }

    def rotateRight: Tree[T] = left match {
      case Node(ll, lr, ldata) =>
        val newRight = Node(lr, right, data)
        Node(ll, newRight, ldata)
      case Empty => this
    }

    def balanceFactor: Int = left.height - right.height

    def insert[A >: T : Ordering](elem: A): Tree[A] = {
      val newTree = if (elem < data) {
        Node(left.insert(elem), right, data)
      } else {
        Node(left, right.insert(elem), data)
      }
      newTree.balance
    }

    def balance: Tree[T] = {
      val bf = balanceFactor
      if (bf > 1) {
        if (left.balanceFactor < 0) Node(left.rotateLeft, right, data).rotateRight
        else rotateRight
      } else if (bf < -1) {
        if (right.balanceFactor > 0) Node(left, right.rotateRight, data).rotateLeft
        else rotateLeft
      } else {
        this
      }
    }

    def print(prefix: String = "", isTail: Boolean = true): String = {
      val builder = new StringBuilder
      builder.append(prefix)
      builder.append(if (isTail) "└── " else "├── ")
      builder.append(data.toString)
      builder.append("\n")
      val newPrefix = prefix + (if (isTail) "    " else "│   ")
      if (left != Empty || right != Empty) {
        if (right != Empty) builder.append(right.print(newPrefix, left == Empty))
        if (left != Empty) builder.append(left.print(newPrefix, true))
      }
      builder.toString()
    }
  }

  case object Empty extends Tree[Nothing] {
    val height: Int = 0
    def rotateLeft: Tree[Nothing] = this
    def rotateRight: Tree[Nothing] = this
    def balanceFactor: Int = 0
    def balance: Tree[Nothing] = this
    def insert[A : Ordering](elem: A): Tree[A] = Node(Empty, Empty, elem)

    def print(prefix: String = "", isTail: Boolean = true): String = {
      val builder = new StringBuilder
      builder.append(prefix)
      builder.append(if (isTail) "└── " else "├── ")
      builder.append("Empty")
      builder.append("\n")
      builder.toString()
    }
  }
}




