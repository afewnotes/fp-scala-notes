
// trait Prop {
//     // 简单的谓词
//     def check: Boolean
// }

// 组合性质，通过 check 检查
def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
}

object Prop {
    type SuccessCount = Int
    type FailedCase = String // 不关心失败的类型，返回失败描述详情即可
}

trait Prop {
    // 失败的场景下返回 Left 即可知道失败原因，失败前有多少次成功
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

case class Gen[A](sample: State[RNG, A]) {
    // exercise 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] = 
        Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
        
    // exercise 8.5
    def unit[A](a: => A): Gen[A] = 
        Gen(State.unit(a))
        
    def boolean: Gen[Boolea] = 
        Gen(State(RNG.boolean))
        
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
        Gen(State.sequence(List.fill(n)(g.sample)))
        
        
    // exercise 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] = 
        Gen(sample.flatMap(a => f(a).sample))
        
    def listOfNViaFlatMap(size: Gen[Int]): Gen[List[A]] = 
        size flatMap (n => Gen.listOfN(n, g))
    
    // exercise 8.7 同类型生成器合成一个
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
        boolean.flatMap(b => if(b) g1 else g2)
    
    // exercise 8.8 根据权重不同，从不同的生成器中取不同的值
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
        val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
        Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
    }
}

// 改造 Prop，通过参数指定多少测试成功才算通过
type TestCases = Int
type Result = Either[(FailedCase, SuccessCount), SuccessCount]
case class Prop(run: TestCases => Result)

// ↑ Left 中的 SuccessCount 与 Right 相等，即 Right 无意义 ---> 直接使用 Option
type Result = Option[(FailedCase, SuccessCount)]

// ↑ None 表示测试通过， Some 表示测试失败 ---> 容易混淆，创造新类型
sealed trait Result {
    def isFalsified: Boolean
}

case object Passed extends Result {
    def isFalsified = false
}

case class Falsified(failure: Failedcase, successes: SuccessCount) extends Result {
    def isFalsified = true
}
// 基于 Gen 随机生成测试用例
case class Prop(run: (TestCases, RNG) => Result) 

def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map{
        case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i) // 失败时记录失败的用例和成功的个数
        } catch {
            // 产生异常记录在结果中
            case e: Exception => Falsified(buildMsg(a, e), i)
        }
    }.find(_.isFalsified).getOrElse(Passed)
}

// 产生一个无限流 A
def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = 
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    
def buildMsg[A](s: A, e: Exception): String = 
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" + 
    s"stack trace: \n ${e.getStackTrace.mkString("\n")}"
    
// e 8.9
case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}


case class SGen[+A](g: Int => Gen[A]) {
    // e. 8.11
    def apply(n: Int): Gen[A] = g
    
    def map[B](f: A => B): SGen[B] = SGen { g(_) map f }
    
    def flatMap[B](f: A => SGen[B]): SGen[B] = {
        val g2: Int => Gen[B] = n => {
            g(n) flatMap { f(_).g(n) }
        }
        
        SGen(g2)
    }
    
    def **[B](s2: SGen[B]): SGen[(A,B)] = {
        SGen(n => apply(n) ** s2(n))
    }
}
case class Gen[+A](sample: State[RNG, A]) {
    // e 8.10
    def unsize: SGen[A] = SGen(_ => this)
}

// e 8.12
def listOf[A](g: Gen[A]): SGen[List[A]] = 
    SGen(n => g.listOfN(n))
    

type MaxSize = Int
case class Prop(run: (MaxSize, TestCases, RNG) => Result)

def forAll[A](g: SGen[A])(f: A => Boolea): Prop = 
    forAll(g(_))(f)
    
def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
        val casesPerSize = (n + max - 1) / max
        val props: Stream[Prop] = 
            Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
            
        val prop: Prop = 
            props.map(p => Prop { (max, _, rn) =>
                p.run(max, casesPerSize, rng)
            }).toList.reduce(_ && _)
            
        prop.run(max, n, rng)
}

def run(p: Prop,
        maxSize: Int = 100,
        testCases: Int = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = 
    p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) => 
            println(s"! Falsified after $n passed tests: \n $msg")
        
        case Passed => 
            println(s"+ OK, passed $testCases tests")
    }
    
// e 8.13
def listOf1[A](g: Gen[A]): SGen[List[A]] = 
    SGen(n => g.listOfN(n max res0))
    
// e 8.14
val sortedProp = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted
    // 排序后，前一元素大于后一元素
    (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
        case (a,b) => a > b
    })
    // 排序后的元素包含所有排序前的元素
    && !ns.exists(!nss.contains(_))
    // 排序前不包含的元素，排序后也不包含
    && !nss.exists(!ns.contains(_))
}