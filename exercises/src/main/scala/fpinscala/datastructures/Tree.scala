package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int=t match{
    case Leaf(_) => 1
    case Branch(l,r) => 1+size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int =t match{
    case Leaf(v)=>v
    case Branch(l,r)=> maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int =t match {
    case Leaf(v) => 0
    case Branch(l,r) => 1 +(depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B]=t match{
    case Leaf(v)=>Leaf(f(v))
    case Branch(l,r)=>Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B=t match{
    case Leaf(v)=>f(v)
    case Branch(l,r)=> g(fold(l)(f)(g),fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int={
    fold(t)(a=>1)(1+_ + _)
  }

   def maximumViaFold(t: Tree[Int]): Int={
     fold(t)(a=>a)(_ max _)
   }

   def depthViaFold[A](t: Tree[A]): Int ={
     fold(t)(a=>0)((d1,d2)=>1+d1 max d2)
   }

   def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B]={
     fold(t)(a=>Leaf(f(a)):Tree[B])((l,r)=>Branch(l,r))
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