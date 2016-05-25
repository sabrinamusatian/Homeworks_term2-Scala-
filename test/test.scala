abstract class AlignmentClass
case class Center() extends AlignmentClass
case class FitWidth() extends AlignmentClass
case class Right() extends AlignmentClass
case class Left() extends AlignmentClass

abstract class ParagraphElement {
}
case class Word (word: String) extends ParagraphElement {}
case class Space (width: Int) extends ParagraphElement {}

object test {
  
  type Line = List[ParagraphElement]
  type Block = List[Line]

  def AddSpaces(words: List[String]): Line = {
    if (words.isEmpty) Nil
    else if (words.tail.isEmpty) new Word(words.head) :: Nil
    else new Word(words.head) :: Space(1) :: AddSpaces(words.tail)
  }

  def AlignString(words: List[String], width: Int, typeOfAlign: AlignmentClass, finalLine: Line): Line = {
    if (typeOfAlign == FitWidth()) {
      val spaces = 1 + width / words.length
      finalLine.map((elem: ParagraphElement) => elem match {
        case Space(1) => new Space(spaces)
        case Word(s) => new Word(s)
      })
    }
    val limit = width / 2
    if (width != 0)
      typeOfAlign match {
        case Center() =>
          if (limit != 0) new Space(width - limit) :: finalLine ::: List(new Space(limit))
          else new Space(width - limit) :: finalLine
        case FitWidth() =>
          if (finalLine.tail.isEmpty) finalLine.head :: new Space(width) :: Nil
          else  finalLine.head :: new Space(width) :: finalLine.tail.tail
        case Right() => new Space(width) :: finalLine
        case Left() => finalLine :+ new Space(width)
      } else finalLine
  }
  
  def Alignment(text: List[String], screenWidth: Int, typeOfAlign: AlignmentClass): Block = {
    val maxLength = text.map(_.length).max
    var newtypeOfAlign = typeOfAlign
    val newScreen = math.max(screenWidth, maxLength)
    if (typeOfAlign == FitWidth())
      newtypeOfAlign = Left()
    def MakeLine(words: List[String], width: Int, str: List[String]): Block = {
      if (words.isEmpty) Nil
      else if (words.tail.isEmpty && words.head.length <= width)
        AlignString((words.head :: str).reverse, width - words.head.length, newtypeOfAlign, AddSpaces((words.head :: str).reverse)) :: Nil
      else if (words.head.length > width)
        AlignString(str.reverse, width + 1, typeOfAlign, AddSpaces(str.reverse)) :: MakeLine(words, newScreen, Nil)
      else MakeLine(words.tail, width - words.head.length - 1, words.head :: str)
    }
    MakeLine(text, newScreen, Nil)
  }
  
  def main(args: Array[String]) {
    print(Alignment(List("I", "am", "a", "good", "student"), 22, FitWidth()))
  }
}
