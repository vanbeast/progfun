package patmat
import Huffman._
object sheet {
    def times(chars: List[Char]): List[(Char, Int)] = {
    	def charIter(chars: List[Char], resultList: List[(Char, Int)]): List[(Char, Int)] = chars match {
          case List() => resultList
          case c :: cs => {
          	charIter(cs, addToList(c, resultList))
    			}
    	}
    	def addToList(c: Char, resultList: List[(Char, Int)]): List[(Char, Int)] = resultList match {
    		case List() => List((c, 1))
    		case x :: xs => if (x._1 == c) (x._1, x._2 + 1) :: xs else x :: addToList(c, xs)
    	}
      charIter(chars, List())
    }                                             //> times: (chars: List[Char])List[(Char, Int)]
    times(List('a', 'b', 'a', 'c', 'b'))          //> res0: List[(Char, Int)] = List((a,2), (b,2), (c,1))
}