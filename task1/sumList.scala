object sum{
  def sumElements(a: List[Int]): Int = {
  	if (a.isEmpty)
  		0
  	else
  		a.head + sumElements(a.tail)
  }                                               //> sumElements: (a: List[Int])Int
}