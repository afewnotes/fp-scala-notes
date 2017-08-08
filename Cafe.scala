
class Cafe {
	
	def buyCoffee(cc: CreditCard): Coffee = {
		val cup = new Coffee()
		// 副作用， 信用卡计费细节
		// 额外发生的行为
		cc.charge(cup.price)

		cup
	}
	
	// 忽略支付细节，更模块化，便于测试；未完全消除副作用
	def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
		val cup = new Coffee()
		p.charge(cc, cup.price)
		
		cup
	}
	
	// 函数式，去除副作用
	def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
		val cup = new Coffee()
		(cup, Charge(cc, cup.price))
	}
	
	case class Charge(cc: CreditCard, amount: Double) {
		def combine(other: Charge): Charge = 
			if (cc = other.cc)
				Charge(cc, amount + other.amount)
			else
				throw new Exception("can't combine charges to different cards")
	}
	
	def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
		val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
		val (coffees, charges) = purchases.unzip
		(coffees, charge.reduce((c1, c2) => c1.combine(c2)))
	}
	
	// 合并同一张卡的费用
	def coalesce(charges: List[Charge]): List[Charge] = {
		charges.groupBy(_.cc).values.map(_.reduce(_ combine _ )).toList
	}
}
