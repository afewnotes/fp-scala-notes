
// 对 Option 的简单扩展，除了知道发生错误，还可以跟踪失败原因
// Option 只能知道发生了错误 (None)
object Either {
    sealed trait Either[+E, +A]
    // 与 Option 类似，也有两个情况，不同的是， Either 两种情况下都有值
    // 互斥并集  disjoint union
    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value:A) extends Eitehr[Nothing, A]
    
    def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
        if (xs.isEmpty)
            Left("mean of empty list")
        else
            Right(xs.sum / xs.length)
            
    // 包含异常堆栈信息
    def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
        try Right(x / y)
        catch { case e: Exception => Left(e) }
    
    def Try[A](a: => A): Either[Exception, A] = 
        try Right(a)
        catch { case e: Exception => Left(e) }
        
    // exercise 4.6 Either 实现 map, flatMap, orElse, map2
    trait Either[+E, +A] {
        def map[B](f: A => B): Either[E, B] = 
            this match {
                case Right(a) => Right(f(a))
                case Left(e) => Left(e)
            }
            
        def flatMap[EE >: E, B](f: A => Either(EE, B)): Either[EE, B] =
            this match {
                case Right(a) => f(a)
                case Left(e) => Left(e)
            }
            
        def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
            this match {
                case Left(_) = b
                case Right(a) = Right(a)
            }
            
        def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
            for {
                a <- this
                b1 <- b
            } yield f(a,b1)
    }
    // for 推导
    def parseInsuranceRateQuote(age:String, numberOfSpeedingTickets: String): Either[Exception,Double] = 
        for {
            a <- Try {age.toInt}
            tickets <- Try { numberOfSpeedingTickets.toInt }
        }
    
    // exercise 4.7
    // 实现 sequence, traverse，遇到错误返回第一个错误
    // def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    //     es match {
    //         case Nil => Right(Nil)
    //         case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    //     }
        
    def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
        es match {
            case Nil => Right(Nil)
            case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
        }
        
    def traverse_1[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
        es.foldRight[Either[E, List[B]]](Right(Nil))((a,b) => f(a).map2(b)(_ :: _))
        
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
        traverse(es)(x => x)
        
    case class Person(name: Name, age: Age)
    sealed class Name(val value: String)
    sealed class Age(val value: Int)
    
    def mkName(name: String): Either[String, Name] = 
        if (name == "" || name == null) Left("Name is empty")
        else Right(new Name(name))
        
    def mkAge(age: Int): Either[String, Age] = 
        if (age < 0) Left("Age is out of range")
        else Right(new Age(age))
        
    def mkPerson(name: String, age: Int): Either[String, Person] = 
        mkName(name).map2(mkAge(age))(Person(_, _))
}