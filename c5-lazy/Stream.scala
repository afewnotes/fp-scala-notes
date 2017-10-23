
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
            case Cons(h, t) => go(t(), h() :: acc)
            case _ => acc
        }
        go(this, List()).reverse
    }
    
    // 改进尾递归最后的 reverse
    // 使用 ListBuffer，一次迭代得出结果
    def toListFast: List[A] = {
        val buf = new collection.mutable.ListBuffer[A]
        @annotation.tailrec
        def go(s: Stream[A]): List[A] = s match {
            case Cons(h, t) => 
                buf += h()
                go(t())
            case _ => buf.toList
        }
        go(this)
    }
    
    // exercise 5.2  take drop
    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 1 => cons(h(), t().take(n-1)) // 获取head，递归处理tail n n-1...
        case Cons(h, _) if n == 1 => cons(h(), empty)
        case _ => empty
    }
    
    @annotation.tailrec
    def drop(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 0 => t().drop(n - 1) // 丢弃head，尾递归处理tail
        case _ => this
    }
    
    // exercise 5.3 takeWhile
    def takeWhile(p: A => Boolean): Stream[A] = this match {
        // case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
        case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p) // without . notation
        case _ => empty
    }
    
    // 函数的描述和求值 分离
    def exists(p: A => Boolean): Boolean = this match {
        // || 非严格求值
        case Cons(h, t) => p(h()) || t().exists(p)  // 显式递归
        case _ => false
    }
                                    // 传名参数
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
            case _ => z
        }
    
    // foldRight 实现 exists
    def existsViaFoldRight(p: A => Boolean): Boolean = 
        foldRight(false)((a, b) => p(a) || b)
        
    // exercise 5.4
    def forAll(p: A => Boolean): Boolean = 
        // this match {
        //     case Cons(h, t) => p(h()) && t().forAll(p)
        //     case _ => true
        // }
        foldRight(true)((a, b) => p(a) && b)
    
    // exercise 5.5
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
        foldRight(empty[A])((h, t) =>
            if (p(h)) cons(h, t)
            else empty
        )
}