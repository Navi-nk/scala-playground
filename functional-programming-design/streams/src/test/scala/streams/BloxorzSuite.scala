package streams

import org.junit._
import org.junit.Assert.assertEquals

import Bloxorz._

class BloxorzSuite {
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  @Test def `neighbours and valid valid neighbours` : Unit =
    new Level1 {
      val testBlock = Block(Pos(5,8), Pos(5,8))
      assertEquals(testBlock.neighbors, List((Block(Pos(5,6),Pos(5,7)),Left), (Block(Pos(5,9),Pos(5,10)),Right), (Block(Pos(3,8),Pos(4,8)),Up), (Block(Pos(6,8),Pos(7,8)),Down)))
      assertEquals(testBlock.legalNeighbors,List((Block(Pos(5,6),Pos(5,7)),Left), (Block(Pos(3,8),Pos(4,8)),Up)))
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }

  @Test def `neighborsWithHistory and newNeighborsOnly test`: Unit =
    new Level1 {
      assertEquals(neighborsWithHistory(Block(Pos(3, 6),Pos(3,6)), List()).toList,
        List((Block(Pos(3,4),Pos(3,5)),List(Left)), (Block(Pos(3,7),Pos(3,8)),List(Right)), (Block(Pos(4,6),Pos(5,6)),List(Down))) )

      assertEquals(newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).to(LazyList),

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      ).toList, List(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ))
    }


  /*@Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }


  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }*/


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
