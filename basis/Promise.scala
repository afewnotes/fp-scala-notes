
// 一个 Future 实例总是和一个 Promise 实例关联在一起
// Future 只读类型，使用计算得到的值
// Promise 将值放入 Future；只能做一次
object PromiseSample {
    
    import concurrent.Promise
    case class TaxCut(reduction: Int)
    // 定义方式二选一
    // 1.指明构造函数的类型参数
    val taxcut = Promise[TaxCut]()
    // 2.指明 val 类型，以便编译器推断
    val taxcut2: Promise[TaxCut] = Promise()
    
    // 获取 Future
    val taxCutF: Future[TaxCut] = taxcut.future
    
    // 调用 success 写入期许的值；写入之后就无法写入其他值了
    taxcut.success(TaxCut(20))

    // Promise 的完成和对返回的 Future 的处理一般发生在不同的线程
    
    object Goverment {
        def redeemCampaignPledge(): Future[TaxCut] = {
            val p = Promise[TaxCut]()
            Future {
                println("starting")
                Thread.sleep(2000)
                p.success(TaxCut(20))
                println("reduced")
            }
            
            p.future
        }
    }
    
    // 兑现诺言 Future
    import scala.util.{Success, Failure}
    val taxCutF: Future[TaxCut] = Goverment.redeemCampaignPledge()
    println("promises..")
    
    taxCutF.onComplete {
        case Success(TaxCut(reduction)) =>
            println(s"wow, cut by $reduction")
        case Failure(ex) =>
            println(s"broke, ${ex.getMessage}")
    }
    
    // 违背诺言 
    case class LameExecure(msg: String) extends Exception(msg)
    object Goverment {
        def redeemCampaignPledge(): Future[TaxCut] = {
            val p = Promise[TaxCut]()
            Future {
                println("starting")
                Thread.sleep(2000)
                p.failure(LameExecure("oho"))  // 相关联的 Future 也会以 Failure 收场
                println("didn't fulfill")
            }
            p.future
        }
    }
    
    // 实践中的场景
    // 1. 非阻塞 I/O
    //      涉及大量 I/O ；数据库连接等；Netty；Promise 实现
    
    // 2. 阻塞 I/O
    //      如数据库交互，可将所有相关代码都放入 Future 代码块
    Future {
        queryDb()
    }
    // 对于隐式的 ExecutionContext，通常建立一个专用的 ExecutionContext 放在数据库层；可以异步的调整线程池来执行调用
    import java.util.concurrent.Executors
    import concurrent.ExecutionContext
    val executorService = Executors.newFiexedThreadPool(5)
    val executionContext = ExecutionContext.fromExecutorService(executorService)
    
    // 3. 长时间运行的计算，同样放入 Future 中 
    
}