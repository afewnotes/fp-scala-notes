
object Sample {
    // 非严格求值
    // 短路 && ||
    // if 判断，可以看做是接收三个参数的函数
    
    def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
        if (cond) onTrue() else onFalse()
        
    if2(a < 22,
        () => println("a"), // 函数字面量语法
        () => println("b")
    )
    // () => A 接收 0 个参数，返回 A 类型; Function0[A] 类型的语法别名
    // ；thunk
    // 在函数中对应的参数称为 传名参数 by name
    
    // 更好的语法  省略括号，直接使用 => A
    def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = 
        if (cond) onTrue() else onFalse()
    
    // 使用时，也不需要特殊处理，正常调用即可
    if2(false, sys.error("false"), 3)
    // scala 会负责将表达式包装为 thunk
    
    // 在方法体中引用的地方都会被求值一次；默认不会缓存参数的求值结果
    def maybeTwice(b: Boolean, i: => Int) = if(b) i+i else 0
    val x = maybeTwice(true, { println("hi"); 1+41 })
    // hi
    // hi
    // x: Int = 84
    
    // 如果想只求值一次，可以使用 lazy 关键字显式缓存这个值
    def maybeTwiceWithCache(b: Boolean, i: => Int) = {
        lazy val j = i
        if (b) j+j else 0
    }
    val y = maybeTwiceWithCache(true, { println("hi"); 1+41 })
    // hi
    // x: Int = 84
    
    // lazy 除了延迟求值外，还会缓存结果，后续引用不会重复求值
}