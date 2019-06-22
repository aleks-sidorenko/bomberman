package com.codenjoy.dojo.bomberman.client

import java.util

import com.codenjoy.dojo.bomberman.model.Elements
import com.codenjoy.dojo.bomberman.model.Elements._
import com.codenjoy.dojo.client.AbstractBoard
import com.codenjoy.dojo.services.{Direction, Point}
import com.codenjoy.dojo.services.PointImpl.pt

import scala.collection.JavaConverters._


case class Board() extends AbstractBoard[Elements] with Cloneable {

  import Helpers._

  override def clone: Board = ???

  def act(action: Action): Board = ???

  def isBarrierAt(x: Int, y: Int): Boolean = getImpassableBlocks.contains(pt(x, y))

  def isBarrierAt(point: Point): Boolean = isBarrierAt(point.getX, point.getY)

  override def valueOf(c: Char): Elements = Elements.valueOf(c)

  override protected def inversionY(y: Int): Int = size - 1 - y

  override protected def withoutCorners: Boolean = true

  override def getAt(x: Int, y: Int): Elements = {
    if (x < 0 || y < 0 || x >= size || y >= size) return WALL
    super.getAt(x, y)
  }

  def getPossibleMoves: Set[Point] = ???

  def getPossibleActions: Set[Action] = ???

  def getPossibleGames(depth: Int = 3): List[Game] = ???

  def getImpassable: Set[Point] = getMeatChoppers ++ getWalls ++ getBombs ++ getDestroyableWalls ++ getOtherBombermans

  def getImpassableBlocks: Set[Point] = getMeatChoppers ++ getWalls ++ getBombs ++ getDestroyableWalls

  //use this method as debug info - prints board information to the console
  override def toString: String =
    s"""
       |Board: $boardAsString
       |My bomberman at: $getMyBomberman
       |Other bombermans at: $getOtherBombermans
       |Meat choppers at: $getMeatChoppers
       |Destroyable walls at: $getDestroyableWalls
       |Bombs at: $getBombs
       |Blasts at: $getBlasts
       |Possible blasts at: $getFutureBlasts
     """.stripMargin

  def getMyBomberman: Point = get(BOMBERMAN, BOMB_BOMBERMAN, DEAD_BOMBERMAN).get(0)

  def getOtherBombermans: Set[Point] = get(OTHER_BOMBERMAN, OTHER_BOMB_BOMBERMAN, OTHER_DEAD_BOMBERMAN).asScala.toSet

  def isMyBombermanDead: Boolean = !get(DEAD_BOMBERMAN).isEmpty

  def getMeatChoppers: Set[Point] = get(MEAT_CHOPPER).asScala.toSet

  def getWalls: Set[Point] = get(WALL).asScala.toSet

  def getDestroyableWalls: Set[Point] = get(DESTROYABLE_WALL).asScala.toSet

  def getBombs: Set[Point] = {
    val result = get(BOMB_TIMER_1).asScala ++ get(BOMB_TIMER_2).asScala ++ get(BOMB_TIMER_3).asScala ++
      get(BOMB_TIMER_4).asScala ++ get(BOMB_TIMER_5).asScala ++
      get(BOMB_BOMBERMAN).asScala ++ get(OTHER_BOMB_BOMBERMAN).asScala
    result.toSet
  }

  def getBlasts: util.Collection[Point] = get(BOOM)

  def addPointNeighbours(point: Point, length: Int = 3): Set[Point] =
    (1 to length).flatMap(i => Set(
      pt(point.getX - i, point.getY),
      pt(point.getX + i, point.getY),
      pt(point.getX, point.getY - i),
      pt(point.getX, point.getY + i)
    )).filter(p => !p.isOutOf(size) && !getWalls.contains(p)).toSet


  def getFutureBlasts: Set[Point] =
    getBombs.flatMap(bomb => getFutureBlasts(bomb))

  def getFutureBlasts(bomb: Point): Set[Point] =
    addPointNeighbours(bomb) ++ Set(bomb)

  def getBlastWithBombAndTimer: Seq[BombWithBlasts] = getBombs.map(b => BombWithBlasts(b, addPointNeighbours(b), getAt(b).ch() - '0')).toSeq

  def getEuclideanDistanceToPoint(from: Point, to: Point): Int =
    Math.abs(from.getX - to.getX) + Math.abs(from.getY - to.getY)

  def getNearestBomberman(from: Point): Option[Point] = getOtherBombermans.toSeq match {
    case Nil => None
    case a@_ => Some(a.minBy(getEuclideanDistanceToPoint(from, _)))
  }

  def getNearestMeatChopper(from: Point): Option[Point] = getMeatChoppers.toSeq match {
    case Nil => None
    case a@_ => Some(a.minBy(getEuclideanDistanceToPoint(from, _)))
  }

  def nextMoveToPoint(from: Point, to: Point): Option[Direction] = closestPathToPoint(from, to).flatMap(_.headOption)

  def closestPathToPoint(from: Point, to: Point): Option[Seq[Direction]] = {
    def tooFarFromPoint(from: Point, to: Point, point: Point): Boolean = {
      val fromX = from.getX
      val fromY = from.getY
      val toX = to.getX
      val toY = to.getY
      val pointX = point.getX
      val pointY = point.getY
      pointX < Math.min(fromX, toX) - 5 || pointX > Math.max(fromX, toX) + 5 ||
        pointY < Math.min(fromY, toY) - 5 || pointY > Math.max(fromY, toY) + 5
    }

    def bfs(from: Point, to: Point): Option[Seq[Direction]] = {
      var visited: Set[Point] = Set(from)
      var queue: List[(Point, Seq[Direction])] = List(from -> Seq.empty)
      var h = 0
      var t = 1
      while (h < t && queue(h)._1 != to) {
        val currentPoint = queue(h)._1
        val currentDirections = queue(h)._2
        h += 1
        val newPoints: Seq[(Point, Direction)] = Direction.onlyDirections().asScala.map(direction => {
          val newX = direction.changeX(currentPoint.getX)
          val newY = direction.changeY(currentPoint.getY)
          pt(newX, newY) -> direction
        })
        newPoints
          .filter(newPoint => !isBarrierAt(newPoint._1) && !visited.contains(newPoint._1) && !tooFarFromPoint(from, to, newPoint._1))
          .foreach(point => {
            queue = queue.:+(point._1 -> (currentDirections ++ Seq(point._2)))
            visited ++= Set(point._1)
            t += 1
          })
      }
      if (h < queue.length && queue(h)._1 == to) Some(queue(h)._2)
      else None
    }

    val res = bfs(from, to)
    res
  }
}

object Helpers {

  def movePoint(x: Int, y: Int, point: Point): Point = createPoint(point.getX + x, point.getY + y)

  def movePoint(d: Direction, point: Point): Point = d match {
    case Direction.DOWN => createPoint(point.getX, point.getY + 1)
    case Direction.LEFT => createPoint(point.getX - 1, point.getY)
    case Direction.UP => createPoint(point.getX, point.getY - 1)
    case Direction.RIGHT => createPoint(point.getX + 1, point.getY)
  }

  def createPoint(x: Int, y: Int): Point = pt(x, y)

}

case class BombWithBlasts(bombLocation: Point, possibleBlasts: Set[Point], timeToBlast: Int)

