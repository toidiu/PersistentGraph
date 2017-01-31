package com.toidiu.graph

import scala.annotation.tailrec

/**
  * Created by toidiu on 1/17/17.
  */

sealed trait Graph[T]

case class PersistGraph[T](nodes: Array[Vertex[T]]) extends Graph[T] {

  override def toString: String = s"""graph:\n${nodes mkString "\n"}"""

  def listVertexIds: Vector[T] = this.nodes.map(v => v.id).toVector

  def getVertex(vertId: T): Option[Vertex[T]] = this.nodes.find(_.id == vertId)

  def getVertex(vert: Vertex[T]): Option[Vertex[T]] = this.nodes.find(_.id == vert.id)

  def getEdges(vertId: T): Option[Array[Edge[T]]] = {
    for {
      v <- this.nodes.find(_.id == vertId)
    } yield v.edges
  }

  def getEdges(vert: Vertex[T]): Option[Array[Edge[T]]] = {
    for {
      v <- this.nodes.find(_.id == vert.id)
    } yield v.edges
  }

  def containsNode(vertId: T): Boolean = this.nodes.exists(_.id == vertId)

  def removeNode(elem: T): PersistGraph[T] = PersistGraph(this.nodes.filter(_.id != elem))

  def addOrUpdateNode(v: Vertex[T]): PersistGraph[T] = {
    val updatedVertices = this.nodes.filter(_.id != v.id) :+ v
    PersistGraph(updatedVertices)
  }

  def meldNodes(n1: T, n2: T): PersistGraph[T] = {
    val optVert1 = this.getVertex(n1)
    val optVert2 = this.getVertex(n2)
    if (optVert1.isDefined && optVert2.isDefined) {
      //combine nodes n1 and n2 and remove any self loops
      val meldedEdges: Array[Edge[T]] = (optVert1.get.edges ++ optVert2.get.edges)
        .filter(e => e.dest != n1 && e.dest != n2)

      //update new n1 node and remove n2
      val noSelfLoopGraph = this.addOrUpdateNode(Vertex(n1, meldedEdges)).removeNode(n2)

      //replace `n2` edge reference with `n1` in all edges
      val n2EdgeVals: Vector[T] = optVert2.get.edges.withFilter(e => e.dest != n1).map(_.dest).toVector
      replaceRef(n1, n2, n2EdgeVals, noSelfLoopGraph)

    } else throw new Exception("one of the nodes doesn't exist")
  }

  @tailrec
  private def replaceRef(ref: T, rmRef: T, vertIds: Vector[T], g: PersistGraph[T]): PersistGraph[T] = {
    vertIds match {
      case h +: t =>
        val edgeVals = g.getEdges(h).getOrElse(Array()).toVector
        val rmRefIdx = edgeVals.indexWhere(_.dest == rmRef)
        if (rmRefIdx >= 0) {
          val updatedEdge = edgeVals.updated(rmRefIdx, Edge(ref))
          val updatedGraph = g.addOrUpdateNode(Vertex(h, updatedEdge.toArray))
          replaceRef(ref, rmRef, t, updatedGraph)
        } else
          replaceRef(ref, rmRef, t, g)
      case _ => g
    }
  }

}

case class Vertex[T](id: T, edges: Array[Edge[T]]) extends Graph[T] {
  override def toString: String = s"""    vertex: $id  edges: ${edges mkString ", "}"""
}

case class Edge[T](dest: T) extends Graph[T] {
  override def toString: String = dest.toString
}


object PersistGraph {
  def apply[T](v: Vertex[T]*): PersistGraph[T] = new PersistGraph(v.toArray)
}

object Vertex {
  def apply[T](id: T, edges: Edge[T]*): Vertex[T] = new Vertex(id, edges.toArray)
}
