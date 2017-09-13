
object Try {
    
    case class Customer(age: Int)
    class Cigarettes
    case class UnderAgeException(message: String) extends Exception(message)
    
    def buyCigarettes(customer: Customer): Cigarettes = 
        if (customer.age < 18) 
            throw UnderAgeException(s"customer must be older than 18 but was ${customer.age}")
        else new Cigarettes
        
    // try/catch 是表达式，会返回一个值，下面代码会返回异常消息
    val youngCustomer = Customer(15)
    try {
        buyCigarettes(youngCustomer)
        "hello"
    } else {
        case UnderAgeException(msg) => msg
    }
        
    // 思考：处理其他线程引发的异常
    // 函数式的错误处理
    
    // Try[A] 表示一种计算
    //      成功：返回类型为 A  的值
    //      异常：返回 Throwable
    // 子类型
    //      Success[A] 代表成功的计算
    //      Failure[A] 代表出错的计算，封装了 Throwable
    
    // 网页爬取示例
    import scala.util.Try
    import java.net.URL
    def parseURL(url: String): Try[URL] = Try(new URL(url))
    // getOrElse 给 Try 提供一个默认值
    val url = parseURL(Console.readLine("URL: ")) getOrElse new URL("http://google.com")
    
    // map
    parseURL("http://google.com").map(_.getProtocol) Success("http")
    parseURL("asdf").map(_.getProtocol) // Failure(xxx)
    // 链接多个 map 会产生嵌套的 Try
    import java.io.InputStream
    // def inputStreamForURL(url: String): Try[Try[Try[InputStream]]] = parseURL(url).map {
    //     u => Try(u.openConnection()).map(conn => Try(conn.getInputStream))
    // }
    
    // flatMap
    def inputStreamForURL(url: String): Try[InputStream] = 
        parseURL(url).flatMap {
            u => Try(u.openConnection()).flatMap(conn => Try(conn.getInputStream))
        }
    
    // filter
    def parseHttpURL(url: String) = parseURL(url).filter(_.getProtocol == 'http')
    parseHttpURL("http://google.com") // Success[URL]
    parseHttpURL("https://google.com") // Failure[URL]
    
    // foreach 
    // 成功时执行副作用 Try 里只有一个元素
    parseHttpURL("http://google.com").foreach(println)
    
    // for
    import scala.io.Source
    def getURLContent(url: String): Try[Iterator[String]] = 
        for {
            url <- parseURL(url)
            connection <- Try(url.openConnection())
            is <- Try(connection.getInputStream)
            source = Source.fromInputStream(is)
        } yield source.getLines()
}