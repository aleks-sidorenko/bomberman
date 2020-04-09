package com.codenjoy.dojo.bomberman

import com.codenjoy.dojo.bomberman.model.Elements

package object client {

  type Score = Double
  type Element = Elements

  def bombFromTimer(t: Int) = t match {
    case t if t > 0 => Elements.valueOf((Elements.BOMB_TIMER_1.ch().toInt + t - 1).toChar)
    case _ => Elements.BOOM
  }

  def bombToTimer(e: Elements): Int = if (e == Elements.BOOM) 0 else e.ch() - '0'
}
