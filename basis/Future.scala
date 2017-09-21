

    // scala.concurrent.Future[T]
    // - 类型容器，代表一种返回值类型为 T 的计算
    // - 计算完成时，可能包含错误
    // - 只能写一次，完成后不能再改变
    // - 提供读取计算值的接口；写入计算值的任务交给 Promise
    
    /**
     * 
     * Future 伴生对象的 apply 方法
     * ExecutionContext 可以执行一个 Future，可看做一个线程池
     * 
     * def apply[T](body: => T)(implicit execctx: ExecutionContext): Future[T]
     * 
     * 第二个参数为隐式参数，如果作用于中存在一个匹配的隐式值，就无需指定这个参数
     * 
     * import scala.concurrent.ExecutionContext.Implicits.global 引入全局执行上下文，确保隐式值的存在
     * 
     * 计算会在 Future 创建后的某个不确定的时间点 由 ExecutionContext 给其分配某个线程来执行
     * 
     */
     
object FutureSample {
    
    // 卡布奇诺。
    import scala.util.Try
    // 类型别名，只是为了让方法签名更有意义
    type CoffeeBeans = String
    type GroundCoffee = String
    case class Water(temperature: Int)
    type Milk = String
    type FrothedMilk = String
    type Espresso = String
    type Cappuccino = String
    
    // 定义步骤
    def grind(beans: CoffeeBeans): GroundCoffee = s"ground coffee of $beans"
    def heatWater(water: Water): Water = water.copy(temperature = 85)
    def frothMilk(milk: Milk): FrothedMilk = s"frothed $milk"
    def brew(coffee: GroundCoffee, heatedWater: Water): Espresso = "espresso"
    def combine(espresso: Espresso, frothedMilk: FrothedMilk): Cappuccino = "cappuccino"
    
    // 可能发生的异常
    case class GrindingException(msg: String) extends Exception(msg)
    case class FrothingException(msg: String) extends Exception(msg)
    case class WaterBoilingException(msg: String) extends Exception(msg)
    case class BrewingException(msg: String) extends Exception(msg)
    
    // 顺序执行
    // 优点：步骤清晰
    // 缺点：下一步必须等待上一步执行完成，才能开始，浪费时间
    def prepareCappuccino(): Try[Cappuccino] = for {
        ground <- Try(grind("arabica beans"))
        water <- Try(heatWater(Water(25)))
        espresso <- Try(brew(ground, water))
        foam <- Try(frothMilk("milk"))
    } yield combine(espresso, foam)
    
    // 改进：同时执行多个步骤
    
    //-------------------
    // 重写 卡布奇诺
    //-------------------
    
    import scala.concurrent.Future
    import scala.concurrent.future
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    import scala.util.Random
    
    def grid(beans: CoffeeBeans): Future[GroundCoffee] = Future {
        println("start grinding...")
        Thread.sleep(Random.nextInt(2000))
        if (beans == "baked beans") throw GrindingException("what?")
        println("finished grinding...")
        s"ground coffee of $beans"
    }
    
    def heatWater(water: Water): Future[Water] = Future {
        println("heating the water now")
        Thread.sleep(Random.nextInt(2000))
        println("Hot!")
        water.copy(temperature = 85)
    }
    
    def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
        println("engaged!")
        Thread.sleep(Random.nextInt(2000))
        println("shutting down")
        s"frothed $milk"
    }
    
    def brew(coffee: GroundCoffee, heatedWater: Water): Future[Espresso] = Future {
        println("brewing")
        Thread.sleep(Random.nextInt(2000))
        println("brewed")
        "espresso"
    }
    
    // 回调
    // Future 的回调是偏函数，
    // 可以把回调传递给 onSuccess 方法，Future 成功则会执行，并将 Future 返回值作为参数传入
    // 类似的，在 onFailure 上注册回调，会在 Future 失败是执行，输入参数为 Throwable
    grind("arabica beans").onSuccess {
        case ground => println("okay")
    }
    
    // 通常将两个回调结合在一起处理，在 onComplete 上注册，输入为 Try
    import scala.util.{Success, Failure}
    grind("baked beans").onComplete {
        case Success(ground) => println(s"got my $ground")
        case Failure(ex) => println("exception")
    }
}