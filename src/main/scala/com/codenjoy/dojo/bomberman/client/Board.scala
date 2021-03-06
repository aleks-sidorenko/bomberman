package com.codenjoy.dojo.bomberman.client

import java.util

import com.codenjoy.dojo.bomberman.model.Elements
import com.codenjoy.dojo.bomberman.model.Elements._
import com.codenjoy.dojo.client.AbstractBoard
import com.codenjoy.dojo.services.{Direction, Point}
import com.codenjoy.dojo.services.PointImpl.pt
import com.codenjoy.dojo.bomberman.client.Point._

import scala.collection.JavaConverters._
import scala.util.Random



class Board() extends AbstractBoard[Elements] with Cloneable { self =>

  override def hashCode(): Int = this.myBomberman.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case board: Board => board.myBomberman == myBomberman
      case _ => false
    }
  }

  override def clone: Board = {
    val board = new Board()
    board.size = self.size
    board.field = Utils.deepCopy3d(self.field)
    board.layersString = self.layersString
    board
  }

  def act(action: Action): Board = {
    val source = myBomberman
    val target = source.move(action.move)
    val board = clone

    // blast bombs
    blastWithBombAndTimer.foreach {
      case BombWithBlasts(_, blasts, t) if t == 0 => // time to bomb
        blasts.foreach { p =>
          board.set(p, if (p == source) DEAD_BOMBERMAN else NONE)
        }
      case BombWithBlasts(p, _, t) => // reduce timer of bomb
        board.set(p, bombFromTimer(t - 1))
    }

    // move bomberman
    if (action.move != Stay && !board.isMyBombermanDead) {
      board.set(target, BOMBERMAN)
      if (board.getAt(source) == BOMBERMAN) board.set(source, NONE)
    }

    board
  }

  def set(point: Point, element: Element): Unit = set(point.getX, point.getY, element.ch())

  def isBarrierAt(x: Int, y: Int): Boolean = impassableBlocks.contains(pt(x, y))

  def isBarrierAt(point: Point): Boolean = isBarrierAt(point.getX, point.getY)

  override def valueOf(c: Char): Elements = Elements.valueOf(c)

  override protected def inversionY(y: Int): Int = size - 1 - y

  override protected def withoutCorners: Boolean = true

  override def getAt(x: Int, y: Int): Elements = {
    if (x < 0 || y < 0 || x >= size || y >= size) return WALL
    super.getAt(x, y)
  }

  def possibleMoves: List[Move] = Random.shuffle((pointNeighbours(myBomberman, 1) &~ impassable).toList.map(p => myBomberman.moveTo(p.getX - myBomberman.getX, p.getY - myBomberman.getY)) ++ (Stay :: Nil))

  def possibleActions: List[Action] = Random.shuffle(possibleMoves.flatMap { m =>
    List(Action(m, NoBomb), Action(m, BombBeforeMove), Action(m, BombAfterMove))
  })

  def impassable: Set[Point] = meatChoppers ++ walls ++ bombs ++ destroyableWalls ++ otherBombermans

  def impassableBlocks: Set[Point] = meatChoppers ++ walls ++ bombs ++ destroyableWalls

  //use this method as debug info - prints board information to the console
  override def toString: String =
    s"""
       |Board: $boardAsString
       |My bomberman at: $myBomberman
       |Other bombermans at: $otherBombermans
       |Meat choppers at: $meatChoppers
       |Destroyable walls at: $destroyableWalls
       |Bombs at: $bombs
       |Blasts at: $blasts
       |Possible blasts at: $futureBlasts
     """.stripMargin

  def myBomberman: Point = get(BOMBERMAN, BOMB_BOMBERMAN, DEAD_BOMBERMAN).get(0)

  def otherBombermans: Set[Point] = get(OTHER_BOMBERMAN, OTHER_BOMB_BOMBERMAN, OTHER_DEAD_BOMBERMAN).asScala.toSet

  def isMyBombermanDead: Boolean = !get(DEAD_BOMBERMAN).isEmpty

  def isMyBombermanKilled: Boolean = isMyBombermanDead || bombBlasts.contains(myBomberman)

  def meatChoppers: Set[Point] = get(MEAT_CHOPPER).asScala.toSet

  def walls: Set[Point] = get(WALL).asScala.toSet

  def destroyableWalls: Set[Point] = get(DESTROYABLE_WALL).asScala.toSet

  def bombs: Set[Point] = {
    val result = get(BOMB_TIMER_1).asScala ++ get(BOMB_TIMER_2).asScala ++ get(BOMB_TIMER_3).asScala ++
      get(BOMB_TIMER_4).asScala ++ get(BOMB_TIMER_5).asScala ++ get(BOOM).asScala
    result.toSet
  }

  def blasts: Set[Point] = get(BOOM).asScala.toSet

  def pointNeighbours(point: Point, length: Int = 3): Set[Point] =
    (1 to length).flatMap(i => Set(
      pt(point.getX - i, point.getY),
      pt(point.getX + i, point.getY),
      pt(point.getX, point.getY - i),
      pt(point.getX, point.getY + i)
    )).filter(p => !p.isOutOf(size) && !walls.contains(p)).toSet


  def futureBlasts: Set[Point] =
    bombs.flatMap(bomb => bombBlasts(bomb))

  def bombBlasts(bomb: Point): Set[Point] =
    pointNeighbours(bomb) ++ Set(bomb)

  def bombBlasts: Set[Point] = blasts.flatMap(p => bombBlasts(p))

  def blastWithBombAndTimer: Seq[BombWithBlasts] = bombs.map(b => BombWithBlasts(b, pointNeighbours(b), bombToTimer(getAt(b)))).toSeq

  def nearestBomberman(from: Point): Option[Point] = otherBombermans.toSeq match {
    case Nil => None
    case a@_ => Some(a.minBy(from.euclideanDistanceToPoint(_)))
  }

  def nearestMeatChopper(from: Point): Option[Point] = meatChoppers.toSeq match {
    case Nil => None
    case a@_ => Some(a.minBy(from.euclideanDistanceToPoint(_)))
  }

  def nearestBomb(from: Point): Option[Point] = bombs.toSeq match {
    case Nil => None
    case a@_ => Some(a.minBy(from.euclideanDistanceToPoint(_)))
  }

  def nearestMeatChopperDistance: Option[Int] = nearestMeatChopper(myBomberman).map(myBomberman.euclideanDistanceToPoint)

  def nearestBombDistance: Option[Int] = nearestBomb(myBomberman).map(myBomberman.euclideanDistanceToPoint)

  def nearestBombermanDistance: Option[Int] = nearestBomberman(myBomberman).map(myBomberman.euclideanDistanceToPoint)

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

  def isMyBombermanBlocked: Boolean = possibleActions.forall(a => a.move == Stay)
}


case class BombWithBlasts(bombLocation: Point, possibleBlasts: Set[Point], timeToBlast: Int)

