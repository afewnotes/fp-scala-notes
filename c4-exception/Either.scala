
// 对 Option 的简单扩展，除了知道发生错误，还可以跟踪失败原因
// Option 只能知道发生了错误 (None)
object Either {
    sealed trait Either[+E, +A]
    // 与 Option 类似，也有两个情况，不同的是， Either 两种情况下都有值
    // 互斥并集  disjoint union
    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value:A) extends Eitehr[Nothing, A]
}