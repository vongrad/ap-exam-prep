import java.util.concurrent._
import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  type Par[A] = ExecutorService => Future[A]
  def run[A] (s: ExecutorService) (a: Par[A]) : Future[A] = a(s)


  case class UnitFuture[A] (get: A) extends Future[A] {
    def isDone = true
    def get (timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel (evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A] (a: A) :Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = a (es)
      val bf = b (es)
      UnitFuture (f(af.get, bf.get))
    }

  def fork[A] (a: => Par[A]) : Par[A] = es => es.submit(
    new Callable[A] { def call = a(es).get }
  )

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4)

  def asyncF[A,B] (f: A => B) : A => Par[B] = a => lazyUnit(f(a))

  // map is shown in the book

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5)

  def sequence[A] (ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A]))((a, z) => map2(a, z)((a, b) => a::b))

  // Exercise 3 (CB7.6)

  // this is shown in the book:

   def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
     val fbs: List[Par[B]] = ps.map(asyncF(f))
     sequence(fbs)
   }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    // as.map(lazyUnit(_)).foldRight(unit(List.empty[A])) ((a, z) => map2(a, z) ((a, b) => if (f(a)) a::b else b))
    map(parMap(as) (a => if (f(a)) List(a) else List.empty[A]))(_.flatten)
  }

  // Exercise 4: implement map3 using map2

  def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) :Par[D]  = {
    map2(pc, map2(pa, pb)((a, b) => (c: C) => f(a, b, c)))((c, g) => g(c))
  }

  // shown in the book

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11)

  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] = es => {
    choices(n(es).get)(es)
  }

  def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] = {
    choiceN(map(cond)(a => if(a) 0 else 1))(List(t, f))
  }

  // Exercise 6 (CB7.13)

  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = es => {
    map(pa)(a => choices(a)(es).get)(es)
  }

  def choiceNviaChooser[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] = {
    chooser(n)(i => choices(i))
  }

  def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] = {
    chooser(cond)(a => if(a) t else f)
  }

  // Exercise 7 (CB7.14)

  def join[A] (a : Par[Par[A]]) :Par[A] = es => {
    a(es).get()(es)
  }

  def flatMap[A, B](pa: Par[A]) (f: A => Par[B]): Par[B] = {
    join(map(pa)(f))
  }

  def join2[A] (ppa: Par[Par[A]]): Par[A] = es => {
    flatMap(ppa)(pa => pa)(es)
  }

  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}

object Test extends App {

  val es: ExecutorService = Executors.newFixedThreadPool(4)
  val l1 = List(0,1,2,3,4,5,6,7,8)

  // Exercise 3
  assert(Par.parFilter(l1)(a => a % 2 == 0)(es).get == List(0,2,4,6,8))

  // Exercise 4
  assert(Par.map3(Par.unit(1), Par.unit(2), Par.unit(3))(_ + _ + _)(es).get == 6)

  // Exercise 5
  assert(Par.choice(Par.unit(true))(Par.unit(1), Par.unit(0))(es).get == 1)
  assert(Par.choice(Par.unit(false))(Par.unit(1), Par.unit(0))(es).get == 0)

  // Exercise 6
  assert(Par.choiceNviaChooser (Par.unit(1)) (List(Par.unit(0), Par.unit(1)))(es).get == 1)
  assert(Par.choiceNviaChooser (Par.unit(0)) (List(Par.unit(0), Par.unit(1)))(es).get == 0)

  assert(Par.choiceViaChooser (Par.unit(true)) (Par.unit(1), Par.unit(0))(es).get() == 1)
  assert(Par.choiceViaChooser (Par.unit(false)) (Par.unit(1), Par.unit(0))(es).get() == 0)

  assert(Par.flatMap(Par.unit(1))(a => Par.unit(a * 8))(es).get == 8)

}
