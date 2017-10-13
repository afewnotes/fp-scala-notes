
object PathDep {
    
    // 路径依赖类型，把只有在运行期才知道的逻辑放在了类型里，编译器可以利用这一点减少甚至防止 bug 的引入
    object Franchise {
        case class Character(name: String)
    }
    
    class Franchise(name: String) {
        import Franchise.Character
        def createFanFiction {
            lovestruck: Character,
            objectOfDesire: Character): (Character, Character) =
            (lovestruck, objectOfDesire)
        }
    }
    // 两个不同的小说
    val starTrek = new Franchise("Star Trek")
    val starWars = new Franchise("Star Wars")
    // 不同小说中的任务
    val quark = Franchise.Character("Quark")
    val jadzia = Franchise.Character("Jadzia Dax")
    val luke = Franchise.Character("Luke Skywalker")
    val yoda = Franchise.Character("Yoda")
   
    // 可能出现的问题：
    // 不同小说中的人物发生关联
    starTrek.createFanFiction(lovestruck = jadzia, objectOfDesire = luke)
    
    // 如何避免
    // 1. 通常做法，快速失败，即在运行期做一些检查，保证角色来自同一小说
    def createFanFiction (lovestruck: Character
        , objectOfDesire: Character): (Character, Character) = {
        // 一旦角色来自不同小说，则抛出异常 IllegalArgumentException
        require(lovestruck.franchise == objectOfDesire.franchise)
        (lovestruck, objectOfDesire)
    }
    
    // 2. scala 的路径依赖类型，在编译期快速失败
    // 将角色和小说之间的联系编码在类型层面上
    class A {
        class B
        val b: Option[B] = None
    }
    val a1 = new A
    val a2 = new A
    val b1 = new a1.B
    val b2 = new a2.B
    a1.b = Some(b1)
    a2.b = Some(b1) //无法编译
    
    // 重写示例
    class Franchise(name: String) {
        case class Character(name: String)
        def createFanFictionWith(
            lovestruck: Character,
            objectOfDesire: Character
            ): (Character, Character) = (lovestruck, objectOfDesire)
    }
    
    val starTrek = new Franchise("Star Trek")
    val starWars = new Franchise("Star Wars")
    val quark = starTrek.Character("Quark")
    val jadiza = starTrek.Character("Jadzia Dax")
    val luke = starWars.Character("Luke Skywalker")
    val yoda = starWars.Character("Yoda")
    // 角色关联
    starTrek.createFanFictionWith(lovestruck = quark, objectOfDesire = jadiza)
    starWars.createFanFictionWith(lovestruck = luke, objectOfDesire = yoda)
    // 错误的角色关联
    starTrek.createFanFictionWith(lovestruck = jadiza, objectOfDesire = luke)
    // 编译失败，提示类型不匹配
    // found   : starWars.Character
    // required: starTrek.Character
    //               starTrek.createFanFictionWith(lovestruck = jadzia, objectOfDesire = luke)
    
    // 依赖方法类型
    // 如果 createFanFictionWith 不在 Franchise 中定义，则可以使用依赖方法类型
    // 一个参数的类型信息依赖于前面的参数
    // 被依赖的实例只能在一个单独的参数列表里
    def createFanFiction(f: Franchise)(lovestruck: f.Character, objectOfDesire: f.Character) =
        (lovestruck, objectOfDesire)
        
    //依赖方法类型通常和抽象类型成员一起使用
    // 场景：键值存储
    object AwesomeDB {
        // 抽象类型，不关心到底是什么形式
        abstract class Key(name: String) {
            type value
        }
    }
    
    import AwesomeDB.key
    class AwesomeDB {
        import collection.mutable.Map
        val data = Map.empty[Key, Any]
        def get(key: Key): Option[key.Value] = data.get(key).asInstanceOf[Option[key.value]]
        // 依赖方法类型，value 的类型必须与 key 对应
        def set(key: Key)(value: key.Value): Unit = data.update(key, value)
    }
    
    // 定义具体的键
    trait IntValued extends Key {
        type Value = Int
    }
    trait StringValued extends Key {
        type Value = String
    }
    object Keys {
        val foo = new Key("foo") with IntValued
        val bar = new Key("bar") with StringValued
    }
    
    // 使用
    val dataStore = new AwesomeDB
    dataStore.set(Keys.foo)(23)
    val i: Option[Int] = dataStore.get(Keys.foo)
    dataStore.set(Keys.foo)("23") // 无法编译
        
    // 实践
    // 配合 cake pattern 一起使用
    // 将运行期才知道的信息编码到类型里
    //  异构列表、自然数的类型级别表示、
    
    // https://github.com/milessabin/shapeless
}