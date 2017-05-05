package misc

/**
  * Created by efim on 05.05.17.
  */
object PrettyLines {
  def main(args: Array[String]) {
    val record = io.Source.stdin.getLines().next().toLowerCase
    println(prettify(record))
  }

  def prettify(record: String): String = {
    if (record.isEmpty || !record.reverse.head.equals('.') || record.exists(char => !char.isLetter && char != '.')) {
      "-1"
    }
    else {
      val sentences = record.split('.').map(_.trim).map(_.split(" +"))
      if (sentences.contains(List())) {
        "-1"
      }
      else {
        val capitalized = sentences.map(list => list.updated(0, list.head.updated(0, list.head.head.toUpper)))
        capitalized.map(_.mkString(" ")).mkString(". ") + '.'
      }
    }
  }
}
