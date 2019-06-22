package com.codenjoy.dojo.bomberman.client

import com.codenjoy.dojo.client.Solver


case class Step(board: Board, action: Action, prev: Option[Step]){
  def next(newAction: Action): Step =
    Step(board = board.act(action), action = newAction, prev = Some(this))
}
object Step {
  def initial(board: Board, action: Action): Step = Step(board, action, None)
}

trait Heuristic {
  def estimate(history: List[Step])(board: Board): Cost
}

class NearestBombermanHeuristic extends Heuristic {
  override def estimate(history: List[Step])(board: Board): Cost = {
    if (board.isMyBombermanDead) Int.MinValue
    else {
      100
    }
  }
}


trait Agent {
  def nextMove(board: Board): Action
}

class AgentImpl(heuristic: Heuristic) extends Solver[Board] with Agent {

  var game: Game = Nil

  final override def get(board: Board): String = {
    nextMove(board).toString
  }

  def updatedGame(board: Board, action: Action) = game.headOption.map(s => Step(board, action, Some(s))).getOrElse(Step.initial(board, action)) :: game

  def nextMove(board: Board): Action = {
    val games = board.getPossibleGames()
    val action = games.headOption.getOrElse(Nil).headOption.map(_.action).getOrElse(Action.default)
    game = updatedGame(board, action)
    action
  }

}

