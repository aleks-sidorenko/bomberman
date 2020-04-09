package com.codenjoy.dojo.bomberman.client

import cats.Monoid
import cats.implicits._
import com.codenjoy.dojo.client.Solver
import com.codenjoy.dojo.bomberman.client.Point._
import com.codenjoy.dojo.services.Point

import scala.collection.mutable


case class Game(board: Board, prev: Option[(Game, Action)], ticks: Int) {
  self =>
  lazy val before: Option[Game] = prev.map { case (g, _) => g }
  lazy val action: Option[Action] = prev.map { case (_, a) => a }

  def tick(newAction: Action): Game =
    new Game(board = board.act(newAction), prev = Some(this -> newAction), ticks + 1)

  def exists(p: Game => Boolean): Boolean = fold(false) { (acc, g) => acc || p(g) }

  def reduce[T: Monoid](f: Game => T): T = fold(Monoid[T].empty) { (acc, g) => Monoid[T].combine(acc, f(g)) }

  def fold[T](zero: T)(f: (T, Game) => T): T = prev match {
    case Some((g, _)) => g.fold(f(zero, this))(f)
    case _ => f(zero, this)
  }

  def myBomb: Option[BombWithBlasts] = action.collect {
    case Action(_, BombBeforeMove) => BombWithBlasts(board.myBomberman, board.bombBlasts(board.myBomberman), 4)
    case Action(move, BombAfterMove) => BombWithBlasts(board.myBomberman.move(move), board.bombBlasts(board.myBomberman), 5)
  }

  def myLastBombGame: Option[Game] = fold[Option[Game]](None) {
    case (Some(b), _) => Some(b)
    case (None, g) => g.myBomb.map(_ => g)
  }

  def canBombNow: Boolean = {
    val game = myLastBombGame
    game.fold(true) { g => self.ticks - g.ticks > g.myBomb.map(_.timeToBlast).getOrElse(5) }
  }

  def myLastBombBlasts: Set[Point] = myLastBombGame.collect {
    case g if self.ticks - g.ticks == g.myBomb.get.timeToBlast => g.myBomb.get.possibleBlasts
  }.getOrElse(Set.empty)

  def myKilledBombermans: Set[Point] = board.otherBombermans & myLastBombBlasts

  def myDestroyedWalls: Set[Point] = board.destroyableWalls & myLastBombBlasts

  def myKilledMeatChoppers: Set[Point] = board.meatChoppers & myLastBombBlasts

  def isMyBombermanKilled: Boolean = board.isMyBombermanKilled

  def findByTick(ts: Int): Option[Game] = {
    if (ts == ticks) Some(this)
    else before.flatMap { g => g.findByTick(ts) }
  }

  lazy val bestGame: Game = {
    implicit val ordering = Ordering.by[Game, Score](_.totalScore)
    val queue = new mutable.PriorityQueue[Game]()
    queue.enqueue(possibleGames:_*)


    def loop(): Unit = {
      val depth = 6
      val startTime = System.nanoTime()
      while (queue.nonEmpty && (System.nanoTime() - startTime)/1000000 <= 50) {
        val best = queue.dequeue
        if (best.ticks - ticks <= depth) {
          queue.enqueue(best.possibleGames:_*)
        }
      }
    }

    loop()

    (for {
      g <- queue.headOption
      g <- g.findByTick(ticks + 1)
    } yield g).getOrElse(tick(Action.default))
  }

  def possibleGames: List[Game] = board.possibleActions.filter(a => canBombNow || a.bomb != NoBomb).map(self.tick)

  def score: Score = {
    var score = Score.zero

    // small bonus for moving
    score += Score.scoreIf(action.exists(a => a.move != Stay)) { Score.moving }

    // small bonus for planting bomb
    score += Score.scoreIf(action.exists(a => a.bomb != NoBomb)) { Score.bomb }

    // if we cannot move - boost to kill ourselves
    score += Score.scoreIf(board.isMyBombermanBlocked) { Score.blocked }

    // penalty for being close to meat chopper
    score += board.nearestMeatChopperDistance.filter(_ <= 2).map(d => (1/(d*d))*Score.meatChopperNear).getOrElse(Score.zero)

    // penalty for being close to bomb
    score += board.nearestBombDistance.filter(_ <= 3).map(d => (1/(d*d))*Score.bombNear).getOrElse(Score.zero)

    // bonus for being close bomberman
    score += board.nearestBombermanDistance.filter(_ <= 3).map(d => (1/(d*d))*Score.bombermanNear).getOrElse(Score.zero)

    // game scores
    score += Score.scoreIf(isMyBombermanKilled) { Score.myBombermanDead }
    score += myKilledBombermans.size * Score.otherBombermanKilled
    score += myKilledMeatChoppers.size * Score.meatChopperKilled
    score += myDestroyedWalls.size * Score.wallDestroyed

    score
  }

  def totalScore: Score = {
    self.reduce {
      _.score
    }
  }
}

object Game {
  def initial(board: Board): Game = new Game(board, None, 1)
}

object Score {
  val zero: Score = 0.0
  val moving: Score  = 2.0
  val bomb: Score  = 5.0
  val blocked: Score  = 50.0

  val meatChopperNear: Score = myBombermanDead/2
  val bombNear: Score = myBombermanDead/3

  val bombermanNear: Score = otherBombermanKilled/5

  val myBombermanDead: Score  = -200.0
  val otherBombermanKilled: Score  = 1000.0
  val meatChopperKilled: Score  = 100.0
  val wallDestroyed: Score  = 10.0


  def scoreIf(p: => Boolean)(s: Score): Score = if (p) s else zero
}


trait Agent {
  def nextMove(board: Board): Action
}

class AgentImpl() extends Solver[Board] with Agent {

  var game: Game = _

  final override def get(board: Board): String = {
    nextMove(board).toString
  }

  private def initGame(board: Board) = if (game == null) game = Game.initial(board)

  def nextMove(board: Board): Action = {
    initGame(board)
    game = game.bestGame
    game.action.getOrElse(Action(Stay, BombBeforeMove))
  }
}

