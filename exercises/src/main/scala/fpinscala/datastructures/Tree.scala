package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int=t match{
    case Leaf(_) => 1
    case Branch(l,r) => 1+size(l) + size(r)
  }


}

object TestTree{
   def main(arr:Array[String]):Unit={
       println("Tree")

       testsize()
   } 

   def testsize()={

       println("---testsize---")

       val t = Leaf(2)

       val s = Tree.size(t)

       println("expected:1")
       println("actual:%d".format(s))

       var t2 = Branch(Leaf(1),Branch(Branch(Leaf(1),Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))),Leaf(3)))

       val s2 = Tree.size(t2)

       println("expected:11")
       println("actual:%d".format(s2))
   }
}