
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    // 计算节点数量
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1 // 叶子节点计1
        case Branch(l, r) => 1 + size(l) + size(r) // 分支节点计1，加上递归左右子树节点数量
    }
    
    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(n) => n
        case Branch(l, r) => maximum(l) max maximum(r)
    }
    
    // 最大深度
    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
    
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
    
    // 泛化实现
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): Tree[B] = t match {
        case Leaf(a) => f(a)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)g)
    }
    
    def sizeViaFold[A](t: Tree[A]): Int = 
        fold(t)(a => 1)(1 + _ + _)
        
    def maximumViaFold(t: Tree[Int]): Int = 
        fold(t)(a => a)(_ max _)
        
    def depthViaFold[A](t: Tree[A]): Int = 
        fold(t)(a => 0)((a, b) => 1 + (a max b))
        
    def mapViaFold[A](t: Tree[A])(f: A => B): Tree[B] = 
        // fold(t)(a => Leaf(f(a)))((l, r) => Branch(l,r))
        fold(t)(a => Leaf(f(a)))(Branch(_, _))
        
}