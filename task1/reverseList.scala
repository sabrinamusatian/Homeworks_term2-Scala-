object reverseList {
  def revers[A](a: List[A]): List[A] = {
  	if (a.isEmpty)
  		List()
  	else
  		List.concat(revers(a.tail), List(a.head))
  }
}