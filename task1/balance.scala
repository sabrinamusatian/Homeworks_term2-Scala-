object balance {
  def balance(chars: List[Char]): Boolean = {
		def Iter(char: List[Char], num: Int): Boolean = {
			if (char.isEmpty){
				num == 0
			}
			else if (char.head == '('){
				Iter(char.tail, num + 1)
			}
			else if (char.head == ')'){
				if (num > 0)
					Iter(char.tail, num - 1)
				else
					false
			}
			else{
				Iter(char.tail, num)
			}
		}
		Iter(chars, 0)
	}                                         //> balance: (chars: List[Char])Boolean
	
}
