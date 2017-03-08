import fpinscala.datastructures._

val list = List(1,2,3,4,5)
val empty = Nil

List.dropWhile(list)(n => n < 4)

List.drop(3, list)

List.sum2(list)

List.length(list)

List.foldLeft(list, 0)((x, y) => x + y)

List.foldLeft(list, 1)((x, y) => x * y)