// Exercise 2.1
def fib(n: Int): Int = 
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int = 
        if n <= 0 then current
        else go(n-1, next, current + next)

    go(n, 0, 1)

val ans = fib(5
)


// Examples 
def findFirst(ss: Array[String], key: String): Int = 
    @annotation.tailrec
    def loop(n: Int): Int = 
        if n >= ss.length then -1
        else if ss(n) == key then n
        else loop(n + 1)

    loop(0)

def poly_findFirst[A](as: Array[A], p: A => Boolean): Int = 
    @annotation.tailrec
    def loop(n: Int): Int = 
        if n >= as.length then -1
        else if p(as(n)) then n
        else loop(n + 1)
    
    loop(0)

val test = poly_findFirst(Array(7, 9, 13), (x: Int) => x == 9)


/** Exercise 2.2: 

    Implement isSorted, which checks whether an Array[A] is sorted according to a
    given comparison function, gt, which returns true if the first parameter is greater
    then the second parameter. */

def my_isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = 
    @annotation.tailrec
    def loop(n: Int, b: Boolean): Boolean = 
        if n+1 >= as.length then b
        else loop(n+1, b && gt(as(n+1), as(n)))   // as.length is not 0

    loop(0, true)

val a = my_isSorted(Array(1,2,3), _ > _)
val b = my_isSorted(Array(1,2,1), _ > _)
val c = my_isSorted(Array(3,2,1), _ < _)
val d = my_isSorted(Array(1,2,1), _ < _)



def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = 
    @annotation.tailrec
    def loop(n: Int): Boolean = 
        if n + 1 >= as.length then true
        else if gt(as(n), as(n+1)) then false
        else loop(n+1)
    loop(0)

val aa = isSorted(Array(1,2,3), _ > _)
val bb = isSorted(Array(1,2,1), _ > _)
val cc = isSorted(Array(3,2,1), _ <
 _)
val dd = isSorted(Array(1,2,1), _ < _)

enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

object List:
    def apply[A](as: A*): List[A] = 
        if as.isEmpty then Nil 
        else Cons(as.head, apply(as.tail*))

