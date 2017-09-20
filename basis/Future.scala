
object Future {
    
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
}