
class Cafe {
	
	def buyCoffee(cc: CreditCard): Coffee = {
		val cup = new Coffee()
		// 副作用， 信用卡计费
		cc.charge(cup.price)

		cup
	}
}
