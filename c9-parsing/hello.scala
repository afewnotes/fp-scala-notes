
//                          协变，可接受任何类型的 Parser
trait Parsers[ParserError, Parser[+_]] {  self => // self 为 trait 实例的引用命名，方便后续内部类明确调用 trait 的方法
    
    // 运行分析器
    def run[A](p: Parser[A])(input: String): Either[ParserError, A]
    
    // 一个仅识别一个字符的分析器
    def char(c: Char): Parser[Char]
    // 对任何 Char c 而言，函数 char 需满足以下法则
    // run(char(c))(c.toString) == Right(c)
    
    // 识别整个字符串
    implicit def string(s: String): Parser[String] 
    // 对任何 String s 而言，函数 string 需满足以下法则
    // run(string(s))(s) == Right(s)

    // 识别两个字符串中的任意一个
    def orString(s1: String, s2: String): Parser[String]
    // 类型更通用
    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    // 在两个字符串出现任一个时都分析成功
    // run(or(string("abcd"), string("1234")))("abcd") == Right("abcd")
    // run(or(string("abcd"), string("1234")))("1234") == Right("1234")
    
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    // 配合string的隐式转换，自动将 String 转换成一个 Parser
    // 这样便可用 "abcd" | "1234" 来创建分析器
    implicit def asStringParser[A](a: A)(implicit f: A =>
        Parser[String]): ParserOps[String] = ParserOps(f(a))
        
    // 类似的二元操作都定义在这里
    case class ParserOPs[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2) // self 消除歧义
        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
        def many[A](p: Parser[A]): Parser[List[A]]
        def map[A,B](a: Parser[A])(f: A => B): Parser[B]
        def slice[A](p: Parser[A]): Parser[String]
        def many1[A](p: Parser[A]): Parser[List[A]]
        def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)[]]
    }
    
    // 重复字符串处理
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
    // 满足期望
    // run(listOfN(3, "ab" | "123"))("abab123") == Right("abab123")
    // run(listOfN(3, "ab" | "123"))("123abab") == Right("123abab")
    
    // val numA: Parser[Int] = char('a').many.map(_.size)
    // run(numA)("aaa") 结果为 Right(3), run(numA)("b") 为 Right(0)
    
    object Laws {
        def equals[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = 
            forAll(in)(s => run(p1)(s) == run(p2)(s))
            
        def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = 
            equals(p, p.map(a => a))(in)
    }
    
    def charViaString(c: Char): Parser[Char] = 
        string(c.toString) map (_.charAt(0))
        
    def succeed[A](a: A): Parser[A] = string("") map (_ => a)
    // run(succeed(a))(s) == Right(a)
}
