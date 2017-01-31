package com.toidiu.graph

import org.specs2._

/**
  * Created by toidiu on 1/8/17.
  */


class Week4Spec extends Specification {
  def is =
    s2"""

    test remove node and listVertexIds size             $s1
    test remove node function                           $s2
    test addOrUpdateNode length and contains            $s3
    test addOrUpdateNode                                $s4

    test meld length and node                           $s5
    test meld no self loops                             $s6
    test meld should replace all edge ref               $s7


      """

  /*

    1         2        3         4
    o    --   o   --   o    --   o
       \    /             \    /
    |         |        |         |
       /   \              /   \
    o    --   o   --   o    --   o
    5         6        7         8

  */
  val ed1 = Edge(1)
  val ed2 = Edge(2)
  val ed3 = Edge(3)
  val ed4 = Edge(4)
  val ed5 = Edge(5)
  val ed6 = Edge(6)
  val ed7 = Edge(7)
  val ed8 = Edge(8)

  val ver1 = Vertex(1, ed2, ed5, ed6)
  val ver2 = Vertex(2, ed1, ed3, ed5, ed6)
  val ver3 = Vertex(3, ed2, ed4, ed7, ed8)
  val ver4 = Vertex(4, ed3, ed7, ed8)
  val ver5 = Vertex(5, ed1, ed2, ed6)
  val ver6 = Vertex(6, ed1, ed2, ed5, ed7)
  val ver7 = Vertex(7, ed3, ed6, ed8)
  val ver8 = Vertex(8, ed3, ed4, ed7)

  val graph1 = PersistGraph(ver1, ver2, ver3, ver4, ver5, ver6, ver7, ver8)
  val graph2: PersistGraph[Int] = graph1.removeNode(ver1.id)



  def s1 = {
    (graph1.listVertexIds.size > graph2.listVertexIds.size) must beTrue
    graph1.listVertexIds.size must beEqualTo(graph2.listVertexIds.size + 1)
  }

  def s2 = graph1.removeNode(ver3.id).listVertexIds.contains(ver3.id) must beFalse

  def s3 = {
    val vertId = 100
    val vertTemp1 = Vertex(vertId)
    val updatedGraph1 = graph1.addOrUpdateNode(vertTemp1)

    updatedGraph1.containsNode(vertId) must beTrue
    (updatedGraph1.getVertex(vertId).get.edges.length == 0) must beTrue
  }

  def s4 = {
    val vertId = 100
    val vertTemp1 = Vertex(vertId)
    val updatedGraph1 = graph1.addOrUpdateNode(vertTemp1)
    updatedGraph1.getVertex(vertId).isEmpty must beTrue

    val vertTemp2 = Vertex(vertId, ed1)
    (updatedGraph1.addOrUpdateNode(vertTemp2).getVertex(vertId).size == 1) must beTrue
  }

  def s5 ={
    val meldGraph = graph1.meldNodes(ver1.id, ver2.id)

    (graph1.listVertexIds.length == meldGraph.listVertexIds.length +1) must beTrue
    meldGraph.containsNode(ver2.id) must beFalse
    meldGraph.containsNode(ver1.id) must beTrue
  }

  def s6 ={
    val meldGraph = graph1.meldNodes(ver1.id, ver2.id)
    val meldV1Edges = meldGraph.getVertex(ver1).get.edges.map(_.dest)

    meldV1Edges.contains(ver1.id) must beFalse
    meldV1Edges.contains(ver2.id) must beFalse
    meldV1Edges.contains(ver5.id) must beTrue
  }

  def s7 ={
    val keepId = ver1.id
    val replaceId = ver2.id
    val meldGraph = graph1.meldNodes(keepId, replaceId)

    val ddd = meldGraph.listVertexIds.forall{ id =>
    val meldV1Edges = meldGraph.getVertex(id).get.edges.map(_.dest)
      val v2EdgeExists = meldV1Edges.contains(replaceId)
      if (v2EdgeExists) println(s"""Bad id: $id""")
      v2EdgeExists == false
    }

    ddd must beTrue
  }

}
