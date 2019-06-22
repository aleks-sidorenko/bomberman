package com.codenjoy.dojo.bomberman.client

import com.codenjoy.dojo.client.WebSocketRunner


object Main extends App {

  private def get(name: String) = System.getenv(s"BOMBERMAN_${name.toUpperCase}")

  def secret = get("secret")

  def code = get("code")

  def url = get("url")

  override def main(args: Array[String]): Unit = {
    val agent = new AgentImpl(new AlgoImpl(new NearestBombermanHeuristic))
    val board = new Board
    WebSocketRunner.runClient(s"http://$url/codenjoy-contest/board/player/$secret?code=$code", agent, board)
  }
}