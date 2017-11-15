// ScalaCheck

// Gen[List[Int]]
val intList = Gen.listOf(Gen.choose(0, 100)) // 从 0 到 100 中生成一个整数列表

val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&  // 检查两次翻转后，恢复初始值
            forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)  // 检查头元素与反转后的尾元素 相同

val failingProp = forAll(intList)(ns => ns.reverse == ns) // 失败测试

// exercise 8.1 求和
val sumProp = forAll(intList)(ns => if (ns.isEmpty) sum(ns) == 0)  // 空列表总和为 0
    && forAll(intList)(ns => sum(List.fill(n)(x)) == n * x)  // 重复元素总和为长度*单个值
    && forAll(intList)(ns => sum(l) == sum(l.reverse)) // 翻转前后的总和相等  交换律
    && forAll(intList)(ns => sum(List(a,b,c)) == sum(List(a)) + sum(List(b,c))) // 结合律
    && forAll(intList)(ns => sum(List(1 to n)) == n*(n+1)/2)
    
    
// exercise 8.2 最大值
val max = forAll(intList)(ns => if (ns.size == 1) max(ns) == ns(0))
    && forAll(intList)(ns => max(ns) >= anyOf(ns))
    && forAll(intList)(ns => ns contains max(ns))
    && forAll(intList)(ns => if (ns.isEmpty) max(ns) == None)
    
