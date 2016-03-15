object pas {
  def pascal(c:Int, r:Int): Int =
  	if ((c == r) || (c == 0))
  		1
  	else
  		pascal(c, r - 1) + pascal(c - 1, r - 1)
}
