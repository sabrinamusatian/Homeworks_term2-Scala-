object filterdef {
  def filter[A](a: List[A], k: A): List[A] = {
  	if (a.isEmpty)
  		List()
  	else if (a.head != k)
  		a.head :: filter(a.tail, k)
  	else
  		filter(a.tail, k)
  }                                              
}