object reverseList {
  def reverse[A](l: List[A]) : List[A] = {
    def reverseHelp[A](list: List[A], res : List[A]) : List[A] =
      if (list.isEmpty) {
        res
      } else {
        reverseHelp(list.tail, list.head :: res)
      }
    reverseHelp(l, List())
  }                                               //> reverse: [A](l: List[A])List[A]
}
