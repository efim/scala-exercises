import misc.PrettyLines
import misc.BlockingSim
//import misc.BlockingSim.{Request, ResourcePool, processRequest}
import misc.BlockingSim.{Request, ResourcePool}

/*
//tests for pretty lines

val line = "hell   o  . foso. yo."
line.exists(char => !char.isLetter && char != '.' && char != ' ')

val sentences = line.split('.').map(_.trim)
val sentLists = sentences.map(_.split(" +"))
  .map(_.toList).foreach(println)
//
PrettyLines.prettify("hellOWOrLD.hELLOWORLD.".toLowerCase)
PrettyLines.prettify("A.B.A.C.A.B.A.".toLowerCase)

val testList = List(1,2,3,4,5,6,7)

testList.slice(1-1,7)

val resources = new ResourcePool(List(0,0,0,0), 0)
val request = new Request(1, 1, 2, 2)

val result = BlockingSim.processRequest(resources, request)
result.resources
result.time

val request2 = new Request(2, 3, 4, 2)
val result2 = BlockingSim.processRequest(resources, request)
result2.resources
result2.time*/
/*

val requests = List(
  new Request(1, 1, 3, 5),
  new Request(2, 4, 6, 2),
  new Request(3, 4, 6, 2),
  new Request(4, 4, 6, 2),
  new Request(5, 1, 5, 1),
  new Request(10, 1, 5, 1)
)

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

val initialResourses = new ResourcePool(List.fill(6)(0), 0)

requests.foldLeft(initialResourses)(processRequest(_,_))
def requestFromArray(input: Array[Int]): Request = new Request(input(0),input(1),input(2),input(3))

val line = "1 1 3 5\n2 4 6 2\n3 4 6 2\n4 4 6 2\n5 1 5 1\n10 1 5 1"
val RequestsFromText = line.split("\n").map(_.split(' '))
  .map(_.map(_.toInt))
  .map(requestFromArray).toList

def f = {
  RequestsFromText.foldLeft(initialResourses)(processRequest(_,_))
  Unit
}

f
*/
