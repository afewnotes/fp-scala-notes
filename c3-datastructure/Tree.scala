
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    // 计算节点数量
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1 // 叶子节点计1
        case Branch(l, r) => 1 + size(l) + size(r) // 分支节点计1，加上递归左右子树节点数量
    }
}