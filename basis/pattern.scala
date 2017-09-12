//
// https://windor.gitbooks.io/beginners-guide-to-scala/content/chp3-pattern-everywhere.html
//

object pattern {
    
    // 一、模式匹配表达式
    //      返回值由第一个匹配的模式中的代码决定
    // e match {
    //     case Pattern1 => block1
    //     case Pattern2 if-clause => block2
    // }
    
    // 实例
    case class Player(name: String, score: Int)
    def message(player: Player) = player match {
        case Player(_, score) if score > 100 =>
            // 去除副作用
            // println("great")
            "great"
        case Player(name, _) =>
            // println("hi, $name")
            "hi, $name"
    }
    def printMessage(player: Player) = println(message(player))
    
    // 二、值定义中的模式
    //      在值定义的左边进行模式匹配；
    //      为避免模式匹配不成功，可只解构那些在编译期就知道类型的样例类
    def currentPlayer(): Player = Player("name", 101)
    
    // val player = currentPlayer()
    val Player(name, _) = currentPlayer()
    doSomething(name)
    
    // 三、for 语句中的模式
    //      所有能在值定义中使用的模式都适用于 for 语句的值定义
    def results(): Seq[(String, Int)] =
        ("a", 100) :: ("b", 99) :: ("c", 101) :: Nil
        
    def hallOfFame = for {
        (name, score) <- results()
        if (score > 100) 
    } yield name
    
    val lists = List(1,2,3) :: List.empty :: List(5,3) :: Nil
    for {
        // @ 操作符，将匹配到的模式绑定到变量
        list @ head :: _ <- lists
    } yield list.size
    // > List[Int] = List(3, 2)
}