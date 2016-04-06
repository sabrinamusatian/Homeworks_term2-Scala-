object addObj {
  def add[A](list: List[A], elem: A): List[A] = {
		if (list.isEmpty) {
			List(elem)
		}
		else {
			list.head :: add(list.tail, elem)
		}
  }                                               //> add: [A](list: List[A], elem: A)List[A]
}
