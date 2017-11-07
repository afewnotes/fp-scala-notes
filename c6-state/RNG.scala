
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
        
    // exercise 6.5 使用 map 重新实现 double
    def doubleViaMap: Rand[Double] = 
        map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
        
    // exercise 6.6 
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
        rng => {
            val (a, r1) = ra(rng)
            val (b, r2) = rb(r1)
            (f(a, b), r2)
        }
        
    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
        map2(ra, rb)((_,_))
        
    val randIntDouble: Rand[(Int, Double)] = 
        both(int, double)
        
    val randDoubleInt: Rand[(Double, Int)] = 
        both(double, int)
        
    // exercise 6.7 sequence
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
        
    def nonNegativeLessThan(n: Int): Rand[Int] = 
        // map(nonNegativeInt){ _ % n } 
        
        // 当结果 不合适时 递归，重试生成器
        // map(nonNegativeInt) { i =>
        //     val mod = i % n
        //     if (i + (n - 1) - mod >= 0) mod else nonNegativeLessThan(n)(...)
        // }
        
        // 显示传递，替代 map
     {   rng =>
            val (i, rng2) = nonNegativeInt(rng)
            val mod = i % n
            if (i + (n - 1) - mod >= 0)
                (mod, rng2)
            else nonNegativeLessThan(n)(rng)
     }
     
     // exercise 6.8
     def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
        rng => {
            val (a, r1) = f(rng)
            g(a)(r1)
        }
        
    def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] = {
        flatMap(nonNegativeInt) { i =>
            val mod = i % n
            if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThanViaFlatMap(n)
        }
    }
    
    // exercise 6.9
    def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = 
        flatMap(s)(a => unit(f(a)))
        
    def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
        flatMap(ra)(a => map2(rb)(b => f(a, b)))
        
    def rollDie: Rand[Int] = map(nonNegativeLessThanViaFlatMap(6)(_ + 1))
    
    // 通用签名
    // def map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S)
    
    // 将 Rand 抽象， 可以处理任何类型的状态
    // 携带状态的计算 / 状态行为 / 状态转换
    type State[S,+A] = S => (A,S)
    //  或 定义成类
    // case class State[S, +A](run: S => (A,S))
    // 将 Rand 定义为 State 的别名
    // type Rand[A] = State[RNG, A]
}

case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = 
        flatMap(a => unit(f(a)))
        
    def map2[B,C](sub: State[S,B])(f: (A,B) => C): State[S,C] = 
        flatMap(a => sub.map(b => f(a,b)))
        
    def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
    })
}

object State {
    type Rand[A] = State[RNG, A]
    
    def unit[S, A](a: A): State[S, A] = 
        State(s => (a, s))
        
    def sequence[S,A](sas: List[State[S,A]]): State[S, List[A]] = {
        def go(s: S, aciton: List[State[S,A]], acc: List[A]): (List[A], S) = 
            actions match {
                case Nil => (ac.reverse, s)
                case h :: t => h.run(s) match { 
                    case (a, s2) => go(s2, t, a :: acc)
                }
            }
            
        State((s: S) => go(s, sas, List()))
    }
    
    def sequenceViaFoldRight[S,A](sas: List[S,A]): State[S, List[A]] = 
        sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
        
    def sequenceViaFoldLeft[S,A](l: List[State[S,A]]): State[S, List[A]] = 
        l.reverse.foldLeft(unit[S, List[A]](List()))((acc,f) => f.map2(acc)(_ :: _))
        
    val ns: Rand[List[Int]] = 
        int.flatMap(x =>
            int.flatMap(y =>
                ints(x).map(xs =>
                    xs.map(_ % y))))
                    
    val ns: Rand[List[Int]] for {
        x <- int
        y <- int
        xs <- ints(x)
    } yield xs.map(_ % y)
    
    
    def modify[S](f: S => S): State[S, Unit] = for {
        s <- get
        _ <- set(f(s))
    } yield()
    
    def get[S]: State[S,S] = State(s => (s,s))
    def set[S](s: S): State[S,Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Iput

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
    def update = (i: Input) => (s: Machine) =>
        (i,s) match {
            case (_, Machine(_, 0, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candy, coin)) => 
                Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
                Machine(true, candy - 1, coin)
        }
        
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
        _ <- sequence(inputs map (modify[Machine] _ compose update))
        s <- get
    } yield (s.coins, s.candies)
}
