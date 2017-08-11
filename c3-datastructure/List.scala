
// 单向链表

// sealed 关键字，约束该特质的所有实现必须定义在这个文件中
// "+" 为型变符号, 参见 note 链接
sealed trait List[+A] // 泛型数据类型
// 空 List 数据构造器
case object Nil extends List[Nothing]
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
        
    // 删除第一个元素
    def tail[A](l: List[A]): List[A] =
        l match {
            case Nil => sys.error("tail of empty list")
            case Cons(_, t) => t
        }

    // 删除前 n 个元素
    def drop[A](l: List[A], n: Int): List[A] = 
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n - 1)
        }
        
    // 删除前 n 个满足条件的元素
    // 调用时，匿名函数中的参数需要使用类型标注
    //      val xs: List[Int] = List(1,2,3,4)
    //      val ex = dropWhile(xs, (x: Int) => x < 3)
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
        l match {
            // pattern guard 在 case 中使用 if 进行条件限制
            case Cons(h, t) if f(h) => dropWhile(t, f)
            case _ => l
        }
        
    // currying 定义；最大化利用类型推导 
    // 调用时匿名函数中的参数不需使用类型标注
    //      val xs: List[Int] = List(1,2,3,4)
    //      val ex = dropWhileC(xs)(x => x < 3)
    // scala 根据参数组里的类信息从左往右传递，因此不需要类型标注
    def dropWhileC[A](l: List[A])(f: A => Boolean): List[A] = 
        l match {
            case Cons(h, t) if f(h) => dropWhileC(t)(f)
            case _ => l
        }

    // 替换 head
    def setHead[A](l: List[A], e: A): List[A] = 
        l match {
            case Nil => sys.error("setHead on empty list")
            case Cons(_, t) => Cons(h, t)
        }
        
    // 将一个列表的元素加到另一个列表后面
    def append[A](a1: List[A], a2: List[A]): List[A] = 
        a1 match {
            case Nil => a2
            case Cons(h, t) => Cons(h, append(t, a2))
        }
        
    // List(1,2,3) -> List(1,2)
    def init[A](l: List[A]): List[A] =
        l match {
            case Nil => sys.error("init of empty list")
            case Cons(h, t) => Cons(h, init(t))  // 递归复制所有元素；每一个元素会使用一个栈帧，大数据量时，栈溢出
            case Cons(h, Nil) => Nil  // 匹配到最后一个元素，直接丢弃
        }
        
    // 使用缓存，将遍历的结果保存，丢弃最后一个元素后，直接返回缓存；避免栈帧过多出现栈溢出
    def init2[A](l: List[A]): List[A] = {
        import collection.mutable.ListBuffer
        
        val buf = new ListBuffer[A]
        def go(cur: List[A]): List[A] = cur match {
            case Nil => sys.error("init of empty list")
            case Cons(h, t) => buf += h; go(t)
            case Cons(h, Nil) => List(buf.toList: _*)
        }
        
        go(l)
    }
        
}


object Test {
    import List._
    
    def main(args: Array[String]) {
        // 模式匹配输出结果 3
        val x = List(1,2,3,4,5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + sum(t)
            case _ => 101
        }
        
        println(x)
        
    }
}