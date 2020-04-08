package com.codenjoy.dojo.bomberman.client

import cats.Monoid
import cats.implicits._
import com.codenjoy.dojo.client.Solver
import com.codenjoy.dojo.bomberman.client.Point._
import com.codenjoy.dojo.services.Point


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

  def myLastBombBlasts: Set[Point] = myLastBombGame.collect {
    case g if self.ticks - g.ticks == g.myBomb.get.timeToBlast => g.myBomb.get.possibleBlasts
  }.getOrElse(Set.empty)

  def myKilledBombermans: Set[Point] = board.otherBombermans & myLastBombBlasts

  def myDestroyedWalls: Set[Point] = board.destroyableWalls & myLastBombBlasts

  def myKilledMeatChoppers: Set[Point] = board.meatChoppers & myLastBombBlasts

  def isMyBombermanKilled: Boolean = board.isMyBombermanDead || myLastBombBlasts.contains(board.myBomberman)

  def findByTick(ts: Int): Option[Game] = {
    if (ts == ticks) Some(this)
    else before.flatMap { g => g.findByTick(ts - 1) }
  }

  def possibleGames(depth: Int = 5): List[Game] = {
    if (depth == 0) Nil
    else {
      board.possibleActions.map(self.tick).flatMap(g => g.possibleGames(depth - 1))
    }
  }

  def score: Score = {
    (if (isMyBombermanKilled) Score.myBombermanDead else 0) +
      (myKilledBombermans.size * Score.otherBombermanKilled) +
      (myKilledMeatChoppers.size * Score.meatChopperKilled) +
      (myDestroyedWalls.size * Score.wallDestroyed)
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
  val myBombermanDead = -50
  val otherBombermanKilled = 1000
  val meatChopperKilled = 100
  val wallDestroyed = 10
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
    val sorted = game.possibleGames().sortBy(_.totalScore).reverse
    (for {
      g <- sorted.headOption
      g <- g.findByTick(game.ticks + 1)
      a <- g.action
    } yield a).getOrElse(Action.default)
  }
}

