val shortList = List(1,2,3,4)
val coolStream: Stream[Option[Char]] = Stream('a','b').map(Some(_)) append Stream.continually(None)

shortList zip coolStream


