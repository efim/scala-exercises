package misc

import jdk.nashorn.internal.ir.RuntimeNode.Request

/**
  * Created by efim on 05.05.17.
  */
object BlockingSim {
  def main(args: Array[String]) {
    val input = io.Source.stdin.getLines()
    val poolSize = input.next().split(' ').head.toInt
    val requests = input.drop(1).map(_.split(' ')).map(_.map(_.toInt)).map(requestFromArray).toList

    requests.reverse.foldLeft(new ResourcePool(List.fill(poolSize)(0), 0))(processRequest(_,_))
  }

  class ResourcePool(val resources: List[Int], val time: Int)

  class Request(val atTime: Int, val leftInd: Int, val rightInd: Int, val forTime: Int)

  def requestFromArray(input: Array[Int]): Request = new Request(input(0),input(1),input(2),input(3))

  def processRequest(resourcePool: ResourcePool, request: Request): ResourcePool = {
    assert(resourcePool.time < request.atTime, "processing request from the past")

    val timeStep = request.atTime - resourcePool.time
    val updatedResources = resourcePool.resources.map(resource => scala.math.max(0, resource - timeStep))

    val isAccepted: Boolean = !updatedResources.slice(request.leftInd - 1, request.rightInd).exists(_ > 0)

    if (isAccepted) {
      println("Yes")
      val (head, rest) = updatedResources splitAt request.leftInd - 1
      val (toAccept, tail) = rest splitAt(request.rightInd - (request.leftInd - 1))

      new ResourcePool(head ::: toAccept.map(_ => request.forTime) ::: tail, request.atTime)
    } else {
      println("No")
      new ResourcePool(updatedResources, request.atTime)
    }
  }
}
