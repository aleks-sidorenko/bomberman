package com.codenjoy.dojo.bomberman.client

import com.codenjoy.dojo.services.{Direction, Point}
import com.codenjoy.dojo.services.PointImpl.pt

object Point {

  def euclideanDistanceToPoint(from: Point, to: Point): Int =
    Math.abs(from.getX - to.getX) + Math.abs(from.getY - to.getY)

  implicit def direction2Move(d: Direction): Move = {
    d match {
      case Direction.DOWN => Down
      case Direction.LEFT => Left
      case Direction.UP => Up
      case Direction.RIGHT => Right
      case _ => Stay
    }
  }

  implicit class PointOps(private val point: Point) extends AnyVal {

    def move(d: Move): Point  =  d match {
      case Down => createPoint(point.getX, point.getY + 1)
      case Left => createPoint(point.getX - 1, point.getY)
      case Up => createPoint(point.getX, point.getY - 1)
      case Right => createPoint(point.getX + 1, point.getY)
    }

    def moveTo(dx: Int, dy: Int): Move = {
      if (dx == 1) Right
      else if (dx == -1) Left
      else if (dy == 1) Down
      else if (dy == -1) Up
      else Stay
    }

    def euclideanDistanceToPoint(to: Point): Int =
      Math.abs(point.getX - to.getX) + Math.abs(point.getY - to.getY)

  }

  def createPoint(x: Int, y: Int): Point = pt(x, y)

}
