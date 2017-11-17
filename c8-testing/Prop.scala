
trait Prop {
    def check: Boolean
}

// 组合性质，通过 check 检查
def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
}

object Prop {
    type SuccessCount = Int
}