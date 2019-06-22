package com.codenjoy.dojo.bomberman.client

sealed trait Move {
  override def toString: String = this match {
    case Left => "LEFT"
    case Right => "RIGHT"
    case Up => "UP"
    case Down => "DOWN"
    case Stay => ""
  }
}
case object Left extends Move
case object Right extends Move
case object Up extends Move
case object Down extends Move
case object Stay extends Move

sealed trait Bomb {
  override def toString: String = this match {
    case NoBomb => ""
    case BombBeforeMove => "ACT,"
    case BombAfterMove =>",ACT"
  }
}
case object NoBomb extends Bomb
case object BombBeforeMove extends Bomb
case object BombAfterMove extends Bomb

case class Action(move: Move, bomb: Bomb = NoBomb) {
  override def toString: String = {
    bomb match {
      case NoBomb => move.toString
      case BombBeforeMove => s"${bomb.toString}${move.toString}"
      case BombAfterMove =>s"${move.toString}${bomb.toString}"
    }
  }
}

object Action {
  val default = Action(Stay, NoBomb)
}
