//
// https://windor.gitbooks.io/beginners-guide-to-scala
//
object extractor {
    
    def main(args: Array[String]) = {
        
        // 中缀表达式
        val xsCons = 1 #:: 2 #:: 4 #:: Stream.empty
        // 其他写法
        // #::(1, #::(2, 4))
        xsCons match {
            case first #:: second #:: _ => first - second
            case _ => -1
        }
        
        // #:: 源码
        object #:: {
            def unapply[A](xs: stream[A]): Option[(A, Stream[A])] = 
                if (xs.isEmpty) None
                else Some((xs.head, xs.tail))
        }
        
        val xs = 3 :: 6 :: 12 :: Nil
        xs match {
            case List(a, b) => a * b
            case List(a, b, c) => a + b + c
            case _ => 0
        }
    
        // 使用通配符
        xs match {
            // 只提取 a, b ，其他元素丢弃
            // 编译期不确定其长度
            case List(a, b, _*) => a * b
            case _ => 0
        }
    }
    
    // 单个参数提取
    def unapply(object: S): Option[T]  
    // 多个参数提取
    def unapply(object: S): Option[(T1, ..., T2)]
    // 布尔提取
    def unapply(object: S): Boolean
    // 序列提取
    def unapplySeq(object: S): Option[Seq[T]]
    // 固定参数和可变参数提取, 至少提取多少个值，其余作为一个可选序列
    def unapplySeq(object: S): Option[(T1,..,Tn-1, Seq[T])]
}

// 匹配名称数据，如：John Doe

// 序列提取实现，取出名字
object GivenNames {
    def unapplySeq(name: String): Options[Seq[String]] = {
        val names = name.trim.split(" ")
        if (names.forall(_.isEmpty)) None
        else Some(names)
    }
    
    def greetWithFirstName(name: String) = name match {
        case GivenNames(firstName, _*) => s"Good morning, $firstname!"
        case _ => "Welcome"
    }
}

// 固定参数和可变参数提取实现，取出姓、名，然后中间名作为序列
object Names {
    def unapplySeq(name: String): Option[(String, String, Seq[String])] = {
        val names = name.trim.split(" ")
        if (names.size < 2) None
        else Some((names.last, names.head, names.drop(1).dropRight(1)))
    }
    
    def greet(fullName: String) = fullName match {
        case Names(lastName, firstName, _*) => s"Hi, $firstName $lastName"
        case _ => "Welcome"
    }
}