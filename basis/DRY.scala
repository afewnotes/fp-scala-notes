

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
            
    // 函数组合
    // 为 sentByOneOf, notSentByAnyOf 结束一个通用的高阶函数
    // 取补
    def complement[A](predicate: A => Boolean) = (a: A) => !predicate(a)
    // 组合
    // f.compose(g) 先调用 g，然后应用 f 到 g 的返回结果上
    // f.andThen(g) 先调用 f, 然后应用 g 到 f 的返回结果上
    
    // 重写 notSentByAnyOf
    val notSentByAnyOf = sentByOneOf andThen (g => complement(g))
    val notSentByAnyOf = sendByOneOf andThen (complement(_))
    
    // 谓词组合
    // 多个过滤器
    // any 返回的新函数检查是否有一个谓词对输入 a 成真
    def any[A](predicates: (A => Boolean)*): A => Boolean =
        a => predicates.exists(pred => pred(a))
    // none 返回的是 any 返回函数的补，只要有一个成真的谓词， none 的条件就无法满足
    def none[A](predicates: (A => Boolean)*) = complement(any(predicates: _*))
    // view 会产生一个延迟集合，后面的 map 操作只会在调用时触发
    // https://stackoverflow.com/questions/6799648/in-scala-what-does-view-do
    // every 利用 none 和 any 来判定是否每个谓词的补对于输入 a 都不成真
    def every[A](predicates: (A => Boolean)*) = none(predicates.view.map(complement(_)): _*)
    
    // 地址为 john@example / 长度小于100 / 大于1000
    val filter: EmailFilter = every(
        notSentByAnyOf(Set("john@example.com")),
        minimumSize(100),
        maximumSize(1000)
        )
        
    // 流水线组合
    // 场景：对用户发送的邮件做一些处理  Email => Email
    val addMissingSubject = (email: Email) =>
        if (email.subject.isEmpty) email.copy(subject = "Empty subject")
        else email
    val checkSpelling = (email: Email) =>
        email.copy(text = email.text.replaceAll("your", "you're"))
    val removeInappropriateLanguage = (email: Email) =>
        email.copy(text = email.text.replaceAll("dynamic typing", "**Censored**"))
    val addAdvertismentToFooter = (email: Email) =>
        email.copy(text = email.text + "\nThis mail sent via Super Awesome Free Mail")
    
    // 通过 addThen 调用 或 Function 伴生对象上的 chain 方法
    val pipeline = Function.chain(Seq(
            addMissingSubject,
            checkSpelling,
            removeInappropriateLanguage,
            addAdvertismentToFooter
        ))
}