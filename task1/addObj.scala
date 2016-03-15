object addObj {
  def add[A](a : List[A], b: A): List[A] = {
  	List.concat(a, List(b))
  }                                         
}