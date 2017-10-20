
case object Empty extends Stream[Nothing]
//                      ↓ 明确求值的 thunk
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    // 智能构造器，首字母小写
    // 跟普通构造器类似，但并不是构造器，而是普通方法
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        // 只在第一次使用时被强制求值，后续调用会返回已缓存的 lazy 值
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }
    
    def empty[A]: Stream[A] = Empty
    
    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
        
}

sealed trait Stream[+A] {
    
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())  // h() 调用，直到这部分真正需要时才求值
    }
    
    // exercise 5.1 Stream -> List
    // 递归方式，大数据量的情况下，容易出现栈溢出
    def toList: List[A] = this match {
        case _ => List()
        case Cons(h, t) => h() :: t().toList()
    }
    
    // 尾递归方式
    def toListTailRec: List[A] = {
        @annotation.tailrec
        def go(s: Stream[A], acc: List[A]): List[A] = s match {
            case cons(h, t) => go(t(), h() :: acc)
            case _ => acc
        }
        go(this, List()).reverse
    }
}