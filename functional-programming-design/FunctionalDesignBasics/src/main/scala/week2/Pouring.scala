package week2

class Pouring(capacity: Vector[Int]) {
  //States
  type State = Vector[Int]
  private val initialState: Vector[Int] = capacity map (_ => 0)

  //moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to:Int) extends Move{
    override def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated( to , state(to) + amount)
    }
  }

  private val glasses: Seq[Int] = capacity.indices

  val allMoves: Seq[Move] =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <-glasses; to <- glasses if from != to) yield Pour(from,to))

  //Paths
  class Path(history: List[Move],val endState: State) {
    //def endState: State = (history foldRight initialState) (_ change _)
   /* def endState: State = trackState(history)
    private def trackState(moves: List[Move]): State = moves match {
      case Nil => initialState
      case move :: xs1 => move change trackState(xs1)
    }*/

    def extend(move:Move) = new Path(move :: history, move change endState)

    override def toString: String =  (history.reverse mkString " ") +"-->" + endState
  }

  val initialPath = new Path(Nil, initialState)

  private def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if(paths.isEmpty) Stream.empty
    else {
      val more: Set[Path] = for {
        path <- paths
        next <- allMoves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val allPaths: Stream[Set[Path]] = from(Set(initialPath), Set(initialState))

  def solution(target: Int): Stream[Path] = {
    for {
      paths <- allPaths
      path <- paths
      if path.endState contains target
    } yield path
  }
}

object Main {
    def main (args: Array[String] ): Unit = {
      val problem = new Pouring(Vector(4,9,19))
      println(problem.allMoves)
      println(problem.allPaths.take(2).toList)

      println(problem.solution(12))
    }

}
