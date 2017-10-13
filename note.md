
> 资料
> - [beginners guide to scala - 中文](https://windor.gitbooks.io/beginners-guide-to-scala)
> - [scala school - Twitter](https://twitter.github.io/scala_school/zh_cn/index.html)
> - [effective scala - Twitter](http://twitter.github.io/effectivescala/index-cn.html)
> - [fpinscala](https://github.com/fpinscala/fpinscala)

- [纯函数](https://zh.wikipedia.org/wiki/%E7%BA%AF%E5%87%BD%E6%95%B0)

    > - 此函数在相同的输入值时，需产生相同的输出。函数的输出和输入值以外的其他隐藏信息或状态无关，也和由I/O设备产生的外部输出无关。
    > - 该函数不能有语义上可观察的函数副作用，诸如“触发事件”，使输出设备输出，或更改输出值以外物件的内容等。
    > - 一个函数在程序的执行过程中除了根据输入参数给出运算结果之外没有其他的影响，就可以说是无副作用的。
 
- [副作用](https://zh.wikipedia.org/wiki/%E5%87%BD%E6%95%B0%E5%89%AF%E4%BD%9C%E7%94%A8)

    > 纯函数内部有隐式（Implicit）的数据流，这种情况叫做副作用（Side Effect）。如 I/O 操作、读取外部变量、改变参数的值等；
    > 有副作用的代码很难测试
    
- 偏函数
    - [PartialFunction](https://www.scala-lang.org/api/current/scala/PartialFunction.html)
    - 一个一元函数，只接受满足条件的值，并允许使用者检查一个给定的输入是否满足条件
    
- 引用透明
    - 表达式可以被它引用的值替代，不依赖上下文，可本地推导
    - 当一个函数所传入参数为引用透明，且函数调用也是引用透明的，那么这个函数就是纯函数。

- [协变、逆变](http://hongjiang.info/scala-covariance-and-contravariance/)
    - O[+A] 协变 ，S 是 P 的子类，则 O[S] 可认为是 O[P] 的子类型
    - O[-A] 逆变 ，S 是 P 的子类，则 O[S] 可认为是 O[P] 的父类型
- [上界、下界](http://hongjiang.info/scala-upper-bounds-and-lower-bounds/)
    - B >: A，类型下界，B 必须是 A 的父类型（包括 A 本身）
    - B <: A, 类型上界，A 必须是 B 的父类型（包括 B 本身）

- 对于不可变的 List 进行操作会产生新的 List，但不会进行内存复制，它们在内存中共享相同的数据，只是简单的引用原始列表
- [模式匹配](http://blog.csdn.net/bluishglc/article/details/51056230)
- call by name, call by value
 
- 异常
    - 破坏了引用透明并引入了上下文依赖
    - 不是类型安全的。如果忘记检测异常，那么异常只能在运行时才被检测到。

- [DRY](http://en.wikipedia.org/wiki/Don%27t_repeat_yourself)
- 高阶函数的三种形式
    - 一个或多个参数是函数，并返回一个值
    - 返回一个函数，但是没有参数是函数
    - 上两个叠加，一个或多个参数是函数，并返回一个函数

- Misc
    - foldLeft  `f(f(f(0,1),2,),3)`
    - foldRight `f(1,f(2,f(3,0)))`