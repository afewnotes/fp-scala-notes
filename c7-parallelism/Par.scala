// 自定义 Future，区别与 juc 包中的 Future，不会阻塞
// 注册一个 callback，在结果准备好后被回调
sealed trait Future[A] {
    // 限制包内访问，不暴露给库的使用者
    // 涉及副作用的部分对外部代码而言是不可见的  【局部副作用】实现 pure api 的细节
    private[pkgname] def apply(k: A => Unit): Unit
}

type Par[+A] = ExecutorService => Future[A]

def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    
    p(es) { a => ref.set(a); latch.countDown } // 获得结果时，将其 set 给 ref, 并释放 latch
    
    latch.await // 等待，知道 ref  被 set
     
    ref.get     // 一旦 latch 被释放，获取结果
}

def unit[A](a: A): Par[A] = 
    es => new Future[A] {
        def apply(cb: A => Unit): Unit =
            cb(a)  // 直接传递 a  给 continuation/callback
    }
    
def fork[A](a: => Par[A]): Par[A] = 
    es => new Future[A] {
        def apply(cb: A => Unit): Unit = 
            eval(es)(a(es)(cb)) // eval分流求值a并立即返回结果；cb将在另外线程中被异步调用
    }
    
// helper 函数使用 ExecutorService 执行异步调用
def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {def call = r})
    
def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
    es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
            // 两个变量分别存储两个结果
            var ar: Option[A] = None
            var br: Option[B] = None
            
            // actor 用来将等待的结果应用 f，并最终传递给 cb
            val combiner = Actor[Either[A,B]](es) {
                
                // A 的结果先到，则等待 B 的结果；
                // A 的结果到后，调用 f 将结果 C 给回调 cb
                case Left(a) => br match {
                    case None => ar = Some(a)
                    case Some(b) => eval(es)(cb(f(a,b)))
                }
                
                // B 的结果先到，则等 A 的结果
                // B 结果到后，调用 f 将结果 C 给回调 cb
                case Right(b) => ar match {
                    case None => br = Some(b)
                    case Some(a) => eval(es)(cb(f(a,b)))
                }
            }
            // 用 Left 封装 a, Right 封装 b，并将二者发给 actor
            // Either类型的用意是说明结果的 来源
            p(es)(a => combiner ! Left(a))
            p2(es)(b => combiner ! Right(b))
        }
    }
    
def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = 
    es => 
        if (run(es)(cond).get) t(es)
        else f(es)
    
// exercise 7.11
def choiceN[A](n: Par[A])(choices: List[Par[A]]): Par[A] = 
    es => {
        val ind = run(es)(n).get 
        run(es)(choices(ind))
    }
    
// exercise 7.12
def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = 
    es => {
        val k = run(es)(key).get
        run(es)(choices(k))
    }
    
// exercise 7.13
// choice Boolean -> Par[A]
// choiceN  Index -> Par[A]
// choiceMap    key -> Par[V]
def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = 
    es => {
        val k = run(es)(pa).get
        run(es)(choices(k))
    }
    
def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = 
    chooser(cond)(if (_) t else f)
    
def choiceNViaChooser[A](n: Par[A])(choices: List[Par[A]]): Par[A] = 
    chooser(n)(choices(_))
    
// exercise 7.14
def join[A](a: Par[[Par[A]]]): Par[A] =
    es => run(es(run(es)(a).get))
    
def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = 
    join(map(p)(f))