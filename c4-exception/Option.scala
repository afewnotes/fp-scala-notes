
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

// O[+A] 协变 ，S 是 P 的子类，则 O[S] 可认为是 O[P] 的子类型
// O[-A] 逆变 ，S 是 P 的子类，则 O[S] 可认为是 O[P] 的父类型
sealed trait Option[+A] {
    
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }
    
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }
    
    // B >: A，类型下界，B 必须是 A 的父类型（包括 A 本身）
    // B <: A, 类型上界，A 必须是 B 的父类型（包括 B 本身）
    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }
    
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case _ => this
    }
    
    def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if(f(a)) => this
        case _ => None
    }
}

object Option {
    
    def mean(xs: Seq[Double]): Option[Double] = 
        if (xs.isEmpty) => None
        else Some(xs.sum / xs.length)
    
    // 方差  avg((x-avg(xs))^2)
    def variance(xs: Seq[Double]): Option[Double] = 
        mean(xs) flatMap (m => mean(xs.map(x => math.power(x - m, 2))))
    
    // 通用模式，调用 map/flatMap/filter 来转换 Option， 最后用 getOrElse 处理错误
    val dept: String = 
        lookupByName("Joe")
        .map(_.dept)
        .filter(_ != "Accounting")
        .getOrElse("Default Dept")
        
    // 抛出异常的常见做法
    o.getOrElse(throw new Exception("Fail"))
    
//
    def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double
    // 对函数进行提升
    // math.abs         Double         => Double
    // lift(math.abs)   Option[Double] => Option[Double]
    // lift(f)返回一个函数，函数对 None 映射为 None， 对 Some 应用 f (即被提升的函数)
    // f 不需要感知 Option 的类型
    
    def parseInsuranceRateQuote(age: String, 
        numberOfSpeedingTickets: String): Option[Double] = {
        val optAge: Option[Int] = Try(age.toInt)
        val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
        insuranceRateQuote(optAge, optTickets)
    }
    
    //           ↓ 非严格求值 / 惰性求值
    def Try[A](a: => A): Option[A] = 
        try Some(a)
        catch {
            case e: Exception => None // 丢弃异常信息
        }
        
    // exercise 4.3
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
        a flatMap (aa => (b map (bb => f(aa, bb))))
    }
    
    // map2 实现 parseInsuranceRateQuote 
    def parseInsuranceRateQuote (age: String,
        numberOfSpeedingTickets: String): Option[Double] = {
        val optAge: Option[Int] = Try { age.toInt } // 等价于：Try(age.toInt) 接收单个参数的函数，在调用时可使用大括号代替小括号
        val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
        map2(optAge, optTickets)(insuranceRateQuote) // 任何一个解析失败，立即返回 None
    }
        
    // exercise 4.4
    // List[Option[A]] -> Option[List[A]]
    // 递归实现 head:tail
    def sequence[A](a: List[Option[A]]): Option[List[A]] = 
        a match {
            case Nil => Some(Nil)
            case h::t => h flatMap (hh => sequence(t) map (hh :: _))
        }
        
    // foldRight 和 map2 实现
    def sequence_1[A](a: List[Option[A]]): Option[List[A]] = 
        a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x, y)(_ :: _))
        
    def parseInts(a: List[String]): Option[List[Int]] = 
        sequence(a map (i => Try(i.toInt)))
        // 效率低，遍历了两次列表，1.对每个字符串转换为 Option[Int]，2.对 Option[Int] 组合成 Option[List[Int]
        
    def traverse[A,B](a: List[A])(f: A => Opiton[B]): Option[List[B]]
}