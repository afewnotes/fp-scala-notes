
object Currying {
    // 部分应用函数 
    //      Partial Applied Function / partial function application
    //      仅传递一部分参数，其他参数留空，生成一个新的函数，其参数列表由留空的参数组成
    // 偏函数 partial function，
    //      函数只处理传入的部分参数，通过 isDefinedAt 来确定该参数是否在函数处理范围内
    
    // 场景：邮件筛选器
    case class Email(
        subject: String,
        text: String,
        sender: string,
        recipient: String
    )
    type EmailFilter = Email => Boolean
    
    //  部分应用函数实现工厂方法
    type IntPairPred = (Int, Int) => Boolean
    def sizeConstraint(pred: IntPairPred, n: Int, email: Email) =
        pred(email.text.size, n) // => Boolean
    
    val gt: IntPairPred = _ > _
    val ge: IntPairPred = _ >= _
    val lt: IntPairPred = _ < _
    val le: IntPairPred = _ <= _
    val eq: IntPairPred = _ == _
    
    // 调用工厂方法
    val minimumSize: (Int, Email) => Boolean = sizeConstraint(ge, _: Int, _: Email) 
    val maximumSize: (Int, Email) => Boolean = sizeConstraint(le, _: Int, _: Email)
    // 对所有为传入值的参数，必须使用占位符 _ ，并指定参数类型

    // 可以绑定或漏掉任意个、任意位置的参数
    val constr20: (IntPairPred, Email) => Boolean = 
        sizeConstraint(_: IntPairPred, 20, _: Email)
    val constr30: (IntPairPred, Email) => Boolean = 
        sizeConstraint(_: IntPairPred, 30, _: Email)
        
    // 从方法得到函数对象
    val sizeConstraintFn: (IntPairPred, Int, Email) => Boolean = sizeConstraint _
    // 在一个方法上做部分应用，不绑定任何参数，这样就产生了一个函数对象
    
    // 柯里化，重新定义工厂方法
    def sizeConstraint(pred: IntPairPred)(n: Int)(email: Email): Boolean = 
        pred(email.text.size, n)
    // 转变为函数对象
    def sizeConstraintFn: IntPairPred => Int => Email => Boolean = sizeConstraint _
    
    // 简化重写
    val minSize: Int => Email => Boolean = sizeConstraint(ge)
    val maxSize: Int => Email => Boolean = sizeConstraint(le)
    // 被留空的参数没必要使用占位符
    
    // 创建 EmailFilter 谓词
    val min20: Email => Boolean = minSize(20)
    val max20: Email => Boolean = maxSize(20)
    
    // 柯里化的函数上直接绑定多个参数
    val min20: Email => Boolean = sizeConstraintFn(ge)(20)
    val max20: Email => Boolean = sizeConstraintFn(le)(20)
    
    // 函数柯里化
    // 通过调用 curried 方法得到
    val sum: (Int, Int) => Int = _ + _
    val sumCurried: Int => Int => Int = sum.curried
    
    // 使用 Function.uncurried 进行反向操作
    
    
    // 函数化依赖注入
    case class User(name: String)
    trait EmailRepository {
        def getMails(user: User, unread: Boolean): Seq[Email]
    }
    
    trait FilterRepository {
        def getEmailFilter(user: User): EmailFilter
    }
    
    trait MailboxService {
        def getNewMails(emailRepo: EmailRepository)(filterRepo: FilterRepository)(user: User) =
            emailRepo.getMails(user, true).filter(filterRepo.getEmailFilter(user))
        
        val newMails: User => Seq[Email]
    }
    
    // 扩展 MailboxService
    object MockEmailRepository extends EmailRepository {
        def getMails(user: User, unread: Boolean): Seq[Email] = Nil
    }
    
    object MockFilterRepository extends FilterRepository {
        def getEmailFilter(user: User): EmailFilter = _ => true
    }
    
    object MailboxServiceWithMockDeps extends MailboxService {
        val newMails: (User) => Seq[Email] = 
            getNewMails(MockEmailRepository)(MockFilterRepository) _
    }
    
    // 调用，无需指定要使用的存储库
    MailboxServiceWithMockDeps.newMails(User("John")) 
}