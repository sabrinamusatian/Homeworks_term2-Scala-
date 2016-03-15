object list {
  def lenList[A](a: List[A]): Int = {
  	if (a.isEmpty)
  		0
  	else
  		1 + lenList[A](a.tail)
  }                                               //> res0: Int = 4
}