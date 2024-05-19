import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered
import scala.reflect.ClassTag

trait Tree[+T] {
  val height: Int
  def rotateLeft: Tree[T]
  def rotateRight: Tree[T]
  def updateHeight: Tree[T]
  def balanceFactor: Int
  def insert[A >: T : Ordering](elem: A): Tree[A]
  protected def balance: Tree[T]
  def print(prefix: String = "", isTail: Boolean = true): String
}

object Tree {
  case class Node[+T](left: Tree[T], right: Tree[T], data: T, height: Int) extends Tree[T] {
    def rotateLeft: Tree[T] = right match {
      case Node(rl, rr, rdata, _) =>
        val newLeft = Node(left, rl, data, 1 + Math.max(left.height, rl.height))
        Node(newLeft, rr, rdata, 1 + Math.max(newLeft.height, rr.height)).updateHeight
      case Empty => this
    }

    def rotateRight: Tree[T] = left match {
      case Node(ll, lr, ldata, _) =>
        val newRight = Node(lr, right, data, 1 + Math.max(lr.height, right.height))
        Node(ll, newRight, ldata, 1 + Math.max(ll.height, newRight.height)).updateHeight
      case Empty => this
    }

    def updateHeight: Tree[T] = {
      val newHeight = 1 + Math.max(left.height, right.height)
      Node(left, right, data, newHeight)
    }

    def balanceFactor: Int = left.height - right.height

    def insert[A >: T : Ordering](elem: A): Tree[A] = {
      val newTree = if (elem < data) {
        Node(left.insert(elem), right, data, height)
      } else {
        Node(left, right.insert(elem), data, height)
      }
      newTree.updateHeight.balance
    }

    def balance: Tree[T] = {
      val bf = balanceFactor
      if (bf > 1) {
        if (left.balanceFactor < 0) Node(left.rotateLeft, right, data, height).rotateRight
        else rotateRight
      } else if (bf < -1) {
        if (right.balanceFactor > 0) Node(left, right.rotateRight, data, height).rotateLeft
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
    def updateHeight: Tree[Nothing] = this
    def balanceFactor: Int = 0
    def balance: Tree[Nothing] = this
    def insert[A : Ordering](elem: A): Tree[A] = Node(Empty, Empty, elem, 1)

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




