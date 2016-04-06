object filter {
  def filter[A](a: List[A], func: A => Boolean): List[A] = {
  	if (a.isEmpty)
  		List()
  	else if (func(a.head))
  		a.head :: filter(a.tail, func)
  	else
  		filter(a.tail, func)
  }                                              
}