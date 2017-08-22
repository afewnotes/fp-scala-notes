
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
        case Cons(x, xs) => x + sum(xs) // 头+尾合计
    }
    
    // 乘积
    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }
    
    // 对 sum 和 product 进行泛化
    // List(1,2,3) -> f(1,f(2,f(3,z)))
    // TODO 是否可以在入参为 0.0 时立即停止递归并返回 0.0
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
        
    def sum2(ns: List[Int]) = 
        foldRight(ns, 0)((x,y) => x + y)
    
    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _)  // (x, y) => x * y 的简写
        
    def length[A](as: List[A]): Int =
        foldRight(as, 0)((_, acc) => acc + 1)
        
    // 尾递归实现
    // List(1,2,3) -> f(f(f(z,1),2),3)
    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }
        
    def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
    
    def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
    
    // List(1,2,3) -> List(3,2,1)
    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((t,h) => Cons(h, t))
    
    //
    // https://stackoverflow.com/a/38881334
    // 推算、简写
    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
        
    // foldLeftViaFoldRight 类似
    
    // appendViaFoldRight   
    def append[A](l: List[A], r: List[A]): List[A] =
        foldRight(l, r)(Cons(_, _))
        
    // 将列表中的列表拼接成单个列表
    def concat[A](l: List[List[A]]): List[A] =
        foldRight(l, Nil:List[A])(append)
        
    // 为列表的每一个元素加 1， 返回一个新列表；纯函数
    def addOne(l: List[Int]): List[Int] =
        foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))
        
    // 3.17
    def doubleToString(l: List[Double]): List[String] =
        foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))
        
    // 3.18 
    // 非栈安全
    def map[A, B](as: List[A])(f: A => B): List[B] = 
        foldRight(as, Nil:List[B])((h, t) => Cons(f(h), t)
    
    def map2[A,B](as: List[A])(f: A => B): List[B] =
        foldRightViaFoldLeft(as, Nil:List[B])((h, t) => Cons(f(h), t))
        
    // 内部使用可变列表实现
    def map3[A,B](as: List[A])(f: A => B): List[B] = {
        val buf = new collection.mutable.ListBuffer[B]
        def go(l: List[A]) =
            l match {
                case Nil => ()
                case Cons(h, t) => buf += f(h); go(t)
            }
        go(as)
        
        // 可变参数与List https://stackoverflow.com/questions/6051302/what-does-colon-underscore-star-do-in-scala
        List(buf.toList: _*)
    }
    
    // 3.19 实现过滤； 与 map 类似
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        foldRight(as, Nil:List[A])((h,t) => if(h) Cons(h, t)) else t
        
    def filter2[A](as: List[A])(f: A => Boolean): List[A] =
        foldRightViaFoldLeft(as, Nil:List[A])((h,t) => if(h) Cons(h,t)) else t
    
    def filter3[A](as: List[A])(f: A => Boolean): List[A] = {
        val buf = new collection.mutable.ListBuffer[A]
        def go(l: List[A]) = 
            l match {
                case Nil => ()
                case Cons(h, t) => if (f(h)) buf += h; go(t)
            }
        
        go(as)
        List(buf.toList: _*)
    }
    
    // 3.20 List 中的每个元素变为 List，然后合并为单个 List
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        concat(map(as)(f))
        // foldRight 方式实现
        // foldRight(as, Nil:List[B])((h,t) => f(h) ::: t)
        // https://stackoverflow.com/questions/6559996/scala-list-concatenation-vs
    
    // 3.21 通过 flatMap 实现 filter
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = 
        flatMap(as)(a => if (f(a)) List(a) else Nil) // 将符合条件的元素变为单个元素的列表，然后拼接
        
    // 3.22 (List(1,2,3), List(4,5,6)) -> List(5,7,9)
    def addList(l: List[Int], r: List[Int]): List[Int] = as match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addList(t1, t2)) // 递归处理每一组元素
    }
    
    // 3.23 泛化add
    def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
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