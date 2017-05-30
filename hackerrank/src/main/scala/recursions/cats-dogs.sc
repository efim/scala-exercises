val range = 1 to 100
val mapped = range.map {
  case num if num%3==0 && num%5==0 => "cats&dogs"
  case num if num%3==0=> "cats"
  case num if num%5==0 => "dogs"
  case num=> num
}

mapped.mkString(" ")

//Scala
val output = (1 to 100).map {
  case num if num%3==0 && num%5==0 => "cats&dogs"
  case num if num%3==0=> "cats"
  case num if num%5==0 => "dogs"
  case num=> num
}.mkString("\n")
println(output)