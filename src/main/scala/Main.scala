import Tree.*
object Main extends App{
 val tree = Tree.Empty.insert(10).insert(3).insert(0).insert(1).insert(4)

  println(tree.print())
}
