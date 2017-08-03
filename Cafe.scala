
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
	

}
