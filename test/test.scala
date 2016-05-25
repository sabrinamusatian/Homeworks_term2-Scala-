abstract class AlignClass
case class ToRight() extends AlignClass
case class ToLeft() extends AlignClass
case class ToMiddle() extends AlignClass
case class ToWidth() extends AlignClass


abstract class ParagraphElement {}

case class Word (word: String) extends ParagraphElement {
  override def toString: String =
    "Word(" + this.word + ")"
}
case class Space (width: Int) extends ParagraphElement {
  override def toString: String =
    "Space(" + this.width.toString + ")"
}

object testWork {
  type Line = List[ParagraphElement]
  type GroupOfLines = List[Line]

  def AddSpaces(words: List[String]): Line = {
    if (words.isEmpty) Nil
    else if (words.tail.isEmpty) new Word(words.head) :: Nil
    else new Word(words.head) :: Space(1) :: AddSpaces(words.tail)
  }
  
  def AlignString(words: List[String], width: Int, alignType: AlignClass, res: Line): Line = {
    if (alignType == ToWidth()) {
      val spaces = 1 + width / words.length
      res.map((elem: ParagraphElement) => elem match {
        case Space(1) => new Space(spaces)
        case Word(s) => new Word(s)
      })
    }
    val limit = width / 2
    if (width != 0)
      alignType match {
        case ToMiddle() =>
          if (limit != 0) new Space(width - limit) :: res ::: List(new Space(limit))
          else new Space(width - limit) :: res
        case ToWidth() =>
          if (res.tail.isEmpty) res.head :: new Space(width) :: Nil
          else  res.head :: new Space(width) :: res.tail.tail
        case ToRight() => new Space(width) :: res
        case ToLeft() => res :+ new Space(width)
      } else res
 }
 def Alignment(text: List[String], screenWidth: Int, alignType: AlignClass): GroupOfLines = {
   def MakeLine(words: List[String], width: Int, str: List[String]): GroupOfLines = {
      if (words.isEmpty) Nil
      else if (words.tail.isEmpty && words.head.length <= width)
        AlignString((words.head :: str).reverse, width - words.head.length, newAlignType, AddSpaces((words.head :: str).reverse)) :: Nil
      else if (words.head.length > width)
        AlignString(str.reverse, width + 1, alignType, AddSpaces(str.reverse)) :: MakeLine(words, newScreen, Nil)
      else MakeLine(words.tail, width - words.head.length - 1, words.head :: str)
    }
    val maxLength = text.map(_.length).max
    var newAlignType = alignType
    val newScreen = math.max(screenWidth, maxLength)
    if (alignType == ToWidth())
      newAlignType = ToLeft()

    MakeLine(text, newScreen, Nil)
  }
  
  def main(args: Array[String]) {
    print(Alignment(List("Look", "at", "this", "funny", "cat", "so", "funny"), 10, ToWidth()))
  }
}
