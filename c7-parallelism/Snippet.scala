object Sample {
    
    // 逐一叠加
    def sum(ints: Seq[Int]): Int = 
        ints.foldLeft(0)((a,b) => a+b)
        
    // 分治
    def sum(ints: IndexedSeq[Int]): Int = // IndexedSeq 标准库中随机访问序列的超类
        if (ints.size <= 1)
            ints.headOption getOrElse 0
        else {
            val (l, r) = ints.splitAt(ints.length / 2) // splitAt 方法基于特定的 index 将自身一分为二
            sum(l) + sum(r) // 递归相加
        }
        
    def sum(ints: IndexedSeq[Int]): Int = 
        if (ints.size <= 1)
            ints headOption getOrElse 0
        else {
            val (l, r) => ints.splitAt(ints.length / 2)
            val sumL: Par[Int] = Par.unit(sum(l)) // 并行计算左半部分
            val sumR: Par[Int] = Par.unit(sum(r)) // 并行计算右半部分
            Par.get(sumL) + Par.get(sumR) // 抽取结果求和
        }
        
    def sum(ints: IndexedSeq[Int]): Par[Int] = 
        if (ints.size <= 1) 
            Par.unit(ints.headOption getOrElse 0)
        else {
            val (l,r) = ints.splitAt(ints.length / 2)
            Par.map2(sum(l), sum(r))(_ + _)
        }
        
    // 显性分流
    def sum(ints: IndexedSeq[Int]): Par[Int] =
        if (ints.length <= 1)
            Par.unit(ints.headOption getOrElse 0)
        else {
            val (l, r) = ints.splitAt(ints.length / 2)
            Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _) // fork 将 Par 分配到另一个独立的逻辑线程中运行
        }
}
    
object Par[A] {
    type Par[A] = ExecutorService => Future[A]
    
    // 并行化单元
    // 接收一个未求值的A，返回结果会在另一个独立的 线程中完成求值
    // def unit[A](a: => A): Par[A]
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
    
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    
    // 从并行计算里抽取结果
    // def get[A](a: Par[A]): A 
    // def run[A](a: Par[A]): A
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
    
    // 通过一个二元函数合并两个并行计算 为 一个新的并行计算
    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
        (es: ExecutorService) => {
            val af = a(es)
            val bf = b(es)
            UnitFuture(f(af.get, bf.get))
        }
        
    // 支持超时设置
    // 创建新的 Future，减去两个线程的耗时
    def map2Timeout[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
        (es: ExecutorService) => 
            val af = a(es)
            val bf = b(es)
            
            new Future[C] {
                def cancel(evenIfRunning: Boolean): Boolean = true
                def isCancelled: Boolean = af.isCancelled || bf.isCancelled
                def isDone: Boolean = af.isDone && bf.isDone
                def get(): C = f(af.get, bf.get)
                def get(timeout: Long, unit: TimeUnit): C = {
                    val start = System.currentTimeMillis
                    val a = af.get(timeout, unit)
                    val elapsed = System.currentTimeMillis - start
                    val remaining = unit.toMillis(timeout) - 
                    val b = bf.get(remaining, unit)
                    f(a, b)
                }
            }
    }
    
    // 标记为 在 run 时进行并发求值
    def fork[A](a: => Par[A]): Par[A] = 
    // 存在的问题：Callable 会阻塞知道内部执行任务完成，阻塞会导致线程 被占用，并行度降低
        es => es.submit(new Callable[A] {
            def call = a(es).get
        })
    
    private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false
    }
    
    // 将函数转换为异步计算
    def asyncF[A,B](f: A => B): A => Par[B] = 
        a => lazyUnit(f(a))
        
    // 排序
    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = 
        map2(parList, unit(()))((a, _) => a.sorted)
        
    // lift  A => B 提升为 Par[A] => Par[B]
    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a,_) => f(a))
        
    // 改进排序
    def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)
    
    // 组合 n 个并行计算
    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
    }
    
    // exercise List[Par[A]] -> Par[List[B]]
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
        ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))
        
    // 尾递归
    def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = 
        ps match {
            case Nil => unit(Nil)
            case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
        }
        
    // 分治
    def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
        if (ps.isEmpty) unit(Vector())
        else if (ps.length == 1) map(as.head)(a => Vector(a))
        else {
            val (l,r) = ps.splitAt(ps.length / 2)
            map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
        }
    }
        
}


// scala 版 java.util.concurrent.ExecutorService

class ExecutorService {
    def submit[A](a: Callable[A]): Future[A]
}
trait Callable[A] { def call: A }
trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
}