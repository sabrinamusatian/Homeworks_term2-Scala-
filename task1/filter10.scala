object filterdef {
  def filter[A](a: List[A], func: A => Boolean): List[A] = {
  	if (a.isEmpty)
  		List()
  	else if (func(a.head))
  		List.concat(List(a.head), filter(a.tail, func))
  	else
  		filter(a.tail, func)
  }                                              
}