

object DRY {
    // 邮件过滤
    case class Email (
        subject: String,
        text: String,
        sender: String,
        recipient: String)
        
    // 类型别名使代码看起来更有意义
    type EmailFilter = Email => Boolean
    // 过滤函数
    def newMailsForUser(mails: Seq[Email], f: EmailFilter) = mails.filter(f)
    
    // 工厂方法配置过滤器
    val sendByOneOf: Set[String] => EmailFilter =
        senders =>
            email => senders.contains(email.sender)
    
    val notSentByAnyOf: Set[String] => EmailFilter =
        senders =>
            email => !senders.contains(email.sender)
            
    val minimumSize: Int => EmailFilter = 
        n =>
            email => email.text.size >= n
            
    val maximumSize: Int => EmailFilter =
        n =>
            email => email.text.size <= n
            
    // 使用
    val emailFilter: EmailFilter = notSentByAnyOf(Set("john@example.com"))
    val mails = Email(
        subject = "it's a title",
        text = "hello",
        sender = "john@example.com",
        recipient = "doe@example.com") :: Nil
    newMailsForUser(mails, emailFilter)
    
    // 重用已有函数，去除 minimumSize，maximumSize 中的重复代码
    // 定义一个函数接受一个谓词函数，检查内容长度是否满足
    type SizeChecker = Int => Boolean
    val sizeConstraint: SizeChecker => EmailFilter =
        f =>
            email => f(email.text.size)
            
    // 重新定义 minimumSize 和 maximumSize
    val minimumSize: Int => EmailFilter =
        n =>
            sizeConstraint(_ >= n)
            
    val maximumSize: Int => EmailFilter =
        n =>
            sizeConstraint(_ <= n)
            
    
}