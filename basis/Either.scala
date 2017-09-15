
object Either {
    
    // Either[A,B] 
    //  要么包含类型 A 的实例 - Left 实例
    //  要么包含类型 B 的实例 - Right 实例
    
    // 语义上并未指定哪个子类代表成功或失败
    // 但一般约定，出现异常是 Left 为出错情况，Right 为成功情况
    
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
    
    
}