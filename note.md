
- [beginners guide to scala - 中文](https://windor.gitbooks.io/beginners-guide-to-scala)

- [fpinscala](https://github.com/fpinscala/fpinscala)

- [纯函数](https://zh.wikipedia.org/wiki/%E7%BA%AF%E5%87%BD%E6%95%B0)

    > - 此函数在相同的输入值时，需产生相同的输出。函数的输出和输入值以外的其他隐藏信息或状态无关，也和由I/O设备产生的外部输出无关。
    > - 该函数不能有语义上可观察的函数副作用，诸如“触发事件”，使输出设备输出，或更改输出值以外物件的内容等。
 
- [副作用](https://zh.wikipedia.org/wiki/%E5%87%BD%E6%95%B0%E5%89%AF%E4%BD%9C%E7%94%A8)

    > 纯函数内部有隐式（Implicit）的数据流，这种情况叫做副作用（Side Effect）。如 I/O 操作，读取外部变量等

- [协变、逆变](http://hongjiang.info/scala-covariance-and-contravariance/)、[上界、下界](http://hongjiang.info/scala-upper-bounds-and-lower-bounds/)

- 对于不可变的 List 进行操作会产生新的 List，但不会进行内存复制，它们在内存中共享相同的数据，只是简单的引用原始列表
- [模式匹配](http://blog.csdn.net/bluishglc/article/details/51056230)
- call by name, call by value

##### 异常
- 破坏了引用透明并引入了上下文依赖
    - 引用透明 （表达式可以被它引用的值替代，不依赖上下文，可本地推导）
- 不是类型安全的。如果忘记检测异常，那么异常只能在运行时才被检测到。
- 