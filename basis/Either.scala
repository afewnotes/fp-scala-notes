
object Either {
    
    // Either[A,B] 
    //  要么包含类型 A 的实例 - Left 实例
    //  要么包含类型 B 的实例 - Right 实例
    
    // 语义上并未指定哪个子类代表成功或失败
    // 但一般约定，出现异常是 Left 为出错情况，Right 为成功情况
    
    // 调用 left / right，就可以得到 LeftProjection / RightProjection
    //  Projection 可调用的方法： map / flatMap / toOption / toSeq
    
    // map 定义在 Projection，但其返回类型是 Either
    
    // 创建
    import scala.io.Source
    import java.net.URL
    def getContent(url: URL): Eigther[String, Source] = 
        if (url.getHost.contains("google"))
            Left("blocked")
        else
            Right(Source.fromURL(url))
            
    // 和 Option, Try 一样，可调用 isLeft / isRight 判断是 Left 值，还是 Right 值
    // 模式匹配
    getContent(new URL("http://google.com")) match {
        case Left(msg) => println(msg)
        case Right(source) => source.getLines.foreach(println)
    }
    
    // content 为 Right
    val content: Either[String, Iterator[String]] = 
        getContent(new URL("http://twitter.com")).right.map(_.getLines())
        
    // 直接返回 Left，不会调用后续的 map
    val moreContent: Either[String, Iterator[String]] = 
        getContent(new URL("http://google.com")).right.map(_.getLines())
        
    // map 操作，会产生嵌套结构
    val t1 = new URL("http://a.com")
    val t2 = new URL("http://b.com")
    val c1 = getContent(t1).right.map(a =>
        getContent(t2).right.map(b => 
            (a.getLines().size + b.getLines().size) / 2
        )
    )
    // =>  Either[String, Either[String, Int]];
    // 可调用 joinRight 使结果扁平化
    
    // flatMap 操作，将结果解包
    val c2 = getContent(t1).right.flatMap(a =>
        getContent(t2).right.map(b =>
            (a.getLines().size + b.getLines().size) / 2
        )
    )
    // => Either[String, Int]
    
    // for
    def averageLineCount(url1: URL, url2: URL): Either[String, Int] = 
        for {
            source1 <- getContent(url1).right
            source2 <- getContent(url2).right
        } yield (source1.getLines().size + source2.getLines().size) / 2
        
    // 缩短 yield
    // 以下代码无法编译通过；
    def averageLineCountWontCompile(url1: URL, url2: URL): Either[String, Int] = 
        for {
            source1 <- getContent(url1).right
            source2 <- getContent(url2).right
            line1 = source1.getLines().size
            line2 = source2.getLines().size
        } yield (line1 + line2) / 2
    // 去掉语法糖，展开后的形式
    def averageLineCountDesugaredWontCompile(url1: URL, url2: URL): Either[String, Int] = 
        getContent(url1).right.flatMap { source1 =>
            getContent(url2).right.map { source2 =>
                val line1 = source1.getLines().size
                val line2 = source2.getLines().size
                (line1, line2)
            }.map{ case(x,y) => (x+y) / 2}  // 外层 flatMap 结果为 Either ，无法调用 map 函数，变易出错
        }
    
    // 将值封装为 Either，然后调用 right （转换为 RightProjection），编译通过
    def averageLineCount2(url1: URL, url2: URL): Either[String, Int] = 
        for {
            source1 <- getContent(url1).right
            source2 <- getContent(url2).right
            line1 <- Right(source1.getLines().size).right
            line2 <- Right(source2.getLines().size).right
        } yield (line1 + line2) / 2
        
    // fold 
    // 接受两个返回类型相同的变换函数，Either 为 Left 时，调用第一个函数，Right 则调用第二个函数
    val contentViaFold: Iterator[String] = 
        getContent(new URL("http://twitter.com")).fold(Iterator(_), _.getLines())
    val moreContentViaFold: Iterator[String]
        getContent(new URL("http://google.com")).fold(Iterator(_), _.getLines())
        
    // 用途
    // 一、错误处理
    // 相对于 Try 可以更具体的处理错误（Try 只能用 Throwable）
    import scala.util.control.Exception.catching
    def handling[Ex <: Throwable, T](exType: Class[Ex])(block: => T): Either[Ex, T] = 
        // 编译期产生的类型总数 Throwable，因此要使用 asInstanceOf 转换
        catching(exType).either(block).asInstanceOf[Either[Ex, T]]
        
    // 将异常放在 Either 里
    import java.net.MalformedURLException
    def parseURL(url: String): Either[MalformedURLException, URL] = 
        handling(classOf[MalformedURLException])(new URL(url))
        
    // 自定义错误类型，使用 Left 封装
    case class Customer(age: Int)
    class Cigarettes
    case class UnderAgeFailure(age: Int, required: Int)
    def buyCigarettes(customer: Customer): Either[UnderAgeFailure, Cigarettes] = 
        if (customer.age < 16) Left(UnderAgeFailure(customer.age, 16))
        else Right(new Cigarettes)
    // 避免使用 Either 处理意料之外的异常，使用 Try 处理更好
    
    // 二、处理集合
    // 依次处理集合元素时，某个元素出现意料之外的结果时，程序不应直接引发异常，使得剩余元素无法处理
    // 黑名单 URL, 
    type Citizen = String
    case class BlackListedResource(url: URL, visitors: Set[Citizen])
    val blacklist = List(
            BlackListedResource(new URL("http://google.com"), Set("a", "b")),
            BlackListedResource(new URL("http://twitter.com"), Set.empty)
        )
}