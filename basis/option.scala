
object OptionSample {
    
    // 创建
    val a: Option[String] = Some("a")
    val b: Option[String] = None
    // 工厂方法
    val c: Option[String] = Option("c") // Some
    val d: Option[String] = Option(null) // None
    
    case class User (
        id: Int,
        name: String,
        age: Int,
        gender: Option[String]
    )
    
    object UserRepository {
        private val users = Map(1 -> User(1, "a", 10, Some("male")), 2 -> User(2, "b", 20, None))
        
        def findById(id: Int): Option[User] = users.get(id)
        
        def findAll = users.values
    }
    
    val user1 = UserRepository.findById(1)
    
    // 如何处理空值 
    
    // 一、通过 isDefined 检测是否有值
    //      太过笨重，可能会在使用前忘记检测
    //      远离这种方式
    if (user1.isDefined) {
        println(user1.get.name)
    }
    
    // 
    val user2 = User(2, "b", 33, None)
    // 二、通过 getOrElse 提供一个默认值
    //      getOrElse 默认参数是一个 传名参数，只有在 Option 值确实为 None 时才会求值
    println("Gender: " + user2.gender.getOrElse("unknown"))
    
    // 三、模式匹配
    //      非常啰嗦，非惯用方式
    user.gender match {
        case Some(gender) => gender
        case None => "unknown"
    }
    
    
// Option 可看做是一种特殊的集合
// 要么只包含一个元素，要么为空

    // 执行一个副作用
    UserRepository.findById(2).foreach(user => println(user.name)) // "b"
    
    // 执行映射
    val age = UserRepository.findById(2).map(_.age) // Some(20)
    
    // flatMap 扁平化
    // val gender = UserRepository.findById(2).map(_.gender) // Option[Option[String]]
    val gender = UserRepository.findById(2).flatMap(_.gender) // Option[String]
    
    // 过滤
    UserRepository.findById(2).filter(_.age > 20)
    
    // for 语句
    // 等同于嵌套的 flatMap 调用
    for {
        user <- UserRepository.findById(1)
        gender <- user.gender
    } yield gender // Some("male")
    
    // 在生成器左侧使用
    for {
        User(_, _, _, Some(gender)) <- UserRepository.findAll
    } yield gender
    
    // 链接 Option 、 链式调用
    // option orElse option orElse ...
}