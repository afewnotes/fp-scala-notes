
trait Prop {
    // 简单的谓词
    def check: Boolean
}

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