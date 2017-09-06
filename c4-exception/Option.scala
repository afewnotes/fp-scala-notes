
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
    
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }
    
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }
    
    // B >: A，协变，B 必须是 A 的父类型（包括 A 本身）
    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }
    
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case _ => this
    }
    
    def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if(f(a)) => this
        case _ => None
    }
}