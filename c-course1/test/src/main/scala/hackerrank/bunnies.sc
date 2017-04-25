val map = Map(2->3, 3->1,5->1)
val map2 = Map(2->1, 3->3,7->1)

val commonKeys = map.keySet ++ map2.keySet
val joinedMap = Map[Int,Int]()

def updateJoined(key:Int):Map[Int,Int] = {
  val power1 = map.getOrElse(key, 0)
  val power2 = map2.getOrElse(key, 0)

  val value = if (power1 > power2) power1 else power2

  joinedMap updated (key, value)
}

commonKeys.foreach(updateJoined(_))

print(joinedMap)

val hopLens = List(2,3,4)

def lcm(nums: List[Int]):Int = {
  val divisorsMaps = nums.map(getDivisorMap)
  val commonDivisorsMap = divisorsMaps.fold(Map())(mergeMaps)
  getLCMFromMap(commonDivisorsMap)
}

def getDivisorMap(n: Int):Map[Int, Int] = ???

def mergeMaps[A](map1: Map[A, Int], map2: Map[A, Int]): Map[A, Int] =
  ???

def getLCMFromMap(map:Map[Int,Int]):Int = ???
