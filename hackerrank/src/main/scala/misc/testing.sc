import misc.PrettyLines

val line = "hell   o  . foso. yo."
val sentences = line.split('.').map(_.trim)
val sentLists = sentences.map(_.split(" +"))
  .map(_.toList).foreach(println)
//
PrettyLines.prettify("hellOWOrLD.hELLOWORLD.".toLowerCase)
PrettyLines.prettify("A.B.A.C.A.B.A.".toLowerCase)
