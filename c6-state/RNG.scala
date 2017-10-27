
trait RNG {
    def nextInt: (Int, RNG)
}

object RNG {
    case class SimpleRNG(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
            val nextRNG = SimpleRNG(newSeed)
            val n = (newSeed >>> 16).toInt
            
            (n, nextRNG)
        }
    }
    
    def randomPair(rng: RNG): ((Int, Int), RNG) = {
        val (i1, rng2) = rng.nextInt
        val (i2, rng3) = rng2.nextInt
        ((i1, i2), rng3)
    }
    
    // exercise 6.1
    // 非负数
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        // Int.MinValue 与 -(Int.MaxValue) 相差 1 
        // i+1 防止出现比最大值还大的结果
        // 即 Int.MinValue 变为 Int.MaxValue， -1 变为 0 ...
        (if (i < 0) -(i+1) else i, r)
    }
    
    // exercise 6.2
    def double(rng: RNG): (Double, RNG) = {
        // val (i, r) = rng.nextInt
        val (i,r) = nonNegativeInt(rng)
        (i / (Int.MaxValue.toDouble + 1), r)
    }
    
    // exercise 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, r) = rng.nextInt
        val (d, r2) = double(r)
        ((i,d), r2)
    }
    
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (d, r) = double(rng)
        val (i, r2) = r.nextInt
        ((d, i), r2)
    }
    
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d, r) = double(rng)
        val (d2, r2) = double(r)
        val (d3, r3) = dobule(r2)
        ((d, d2, d3), r3)
    }
    
    // exercise 6.4
    // 递归实现
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        // 边际情况
        if (count == 0) (List(), rng)
        else {
            val (x, r) = rng.nextInt
            val (xs, r2) = ints(count - 1)(r)
            (x :: xs, r2)
        }
    }
    
    // 尾递归实现
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
            if (count == 0) (xs, r)
            else {
                val (x, r2) = r.nextInt
                go(count - 1, r2, x :: xs)
            }
        }
        
        go(count, rng, List())
    }
    
    // 类型别名
    // 随机生成 A 类型值的类型
    // RNG 生成一个 A，同时转化 RNG 为一个新的状态，以便后续使用
    type Rand[+A] = RNG => (A, RNG)
    
    // RNG 的 nextInt 方法变成新类型的一个值
    val int: Rand[Int] = _.nextInt
    
    // 避免显式的传递状态
    // 返回常量值
    def unit[A](a:A): Rand[A] =
        rng => (a, rng)
    // 转换状态行为的输出而不修改状态本身
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
        rng => {
            val (a, rng2) => s(rng)
            (f(a), rng2)
        }
        
    // 大于0且被2整除
    def nonNegativeEven: Read[Int] = 
        map(nonNegativeInt)(i => i - i % 2)
        
    // exercise 6.2 使用 map 重新实现 double
    def doubleViaMap: Rand[Double] = 
        map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
}