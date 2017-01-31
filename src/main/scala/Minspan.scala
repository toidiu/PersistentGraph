package com.toidiu.graph

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * Created by toidiu on 1/17/17.
  */
object MinSpan {

  private val rand = Random

  def run = {
    val graph = loadGraph("sampleGraph.txt")
    println(graph)

    val futList = Future.traverse(0 to 1)(i => kragerMinCut(graph))
    futList.map(list => println(s"""The final min cut is: ${list.min}""".stripMargin))
  }


  def kragerMinCut(graph: PersistGraph[Int]): Future[Int] = {
    val vertIds = graph.listVertexIds
    val vertLen = vertIds.length
    // check if # vertices is 2 then calculate the crossEdges
    if (vertLen == 2){
      Future(graph.nodes(0).edges.length)
    }
    else {
      val (n1: Int, n2: Int) = getRandNodes(vertIds, vertLen, graph)
      val meldGraph = Future(graph.meldNodes(n1, n2))
      meldGraph.flatMap(g => kragerMinCut(g))
    }
  }

  private def getRandNodes(vertIds: Vector[Int], vertLen: Int, g: PersistGraph[Int]): (Int, Int) = {
    //generate two random indicei into the graph keys
    val r1 = rand.nextInt(vertLen)
    val edges = g.getEdges(vertIds(r1)).get
    val r2 = rand.nextInt(edges.length)
    (vertIds(r1), edges(r2).dest)
  }

  private def loadGraph(fileName: String): PersistGraph[Int] = {
    val lines = scala.io.Source.fromFile(fileName).getLines()
    val vertArr: ArrayBuffer[Vertex[Int]] = ArrayBuffer()

    for (line <- lines) {
      val edges = line.split("\t")

      val vertId: Int = edges(0).toInt
      val edgeArr: Array[Edge[Int]] = edges.drop(1).map(e => Edge(e.toInt))
      vertArr.append(Vertex(vertId, edgeArr))
    }

    PersistGraph(vertArr.toArray)
  }
}
