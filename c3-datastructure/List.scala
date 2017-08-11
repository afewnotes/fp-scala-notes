
// 单向链表

// sealed 关键字，约束该特质的所有实现必须定义在这个文件中
sealed trait List[+A] // 泛型数据类型
// 空 List 数据构造器
case Object Nil extends List[Nothing]
// 非空 List 构造器
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/*

以下两个链表相同，表示同一个内存里的数据结构

List("a", "b")
Cons("a", Cons("b", Nil)

*/

// 伴生对象，包含创建 List 和对 List 操作的一些函数
object List {
    // 总计
    def sum(ints: List[Int]): Int = ints match { // 模式匹配
        case Nil => 0
        case Cons(x, xs) => x + sum(xs) // 头+尾部合计
    }
    
    // 乘积
    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }
    
    // 可变参数
    def apply[A](as: A*): List[A] = 
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}