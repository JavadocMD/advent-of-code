package aoc2020

def toChunks(input: Array[String]): Array[Array[String]] =
  var result = List.empty[Array[String]]
  var i0     = 0
  var i1     = input.indexOf("", i0)
  while (i1 >= 0) {
    result ::= input.slice(i0, i1)
    i0 = i1 + 1
    i1 = input.indexOf("", i0)
  }
  if (i0 < input.length) {
    result ::= input.slice(i0, input.length)
  }
  result.reverse.toArray

/** Read input in batches, with batches separated by empty lines. Each batch will be combined with the given `foldFn`
  * into a single string.
  */
def batchIterator[B](mapFn: (String => B), reduceFn: (B, B) => B)(input: Iterable[String]): Iterator[B] = {
  val lines = input.iterator

  new Iterator[B]() {
    def hasNext = lines.hasNext
    def next() = {
      // Might result in odd behavior if there are multiple blank lines in a row,
      // and if `mapFn` is not well-defined for the empty string.
      var curr   = lines.next()
      var result = mapFn(curr)
      while (curr != "") {
        result = reduceFn(result, mapFn(curr))
        curr = lines.next()
      }
      result
    }
  }
}

/** Iterates over all sequential pairs. Like `sliding(2)`, but with tuples and doesn't have a final 1-sized entry.
  */
def pairsIterator[A](xs: Iterable[A]): Iterator[(A, A)] = {
  if (xs.isEmpty) Iterator.empty
  else {
    val underlying = xs.iterator
    var prev       = underlying.next()

    new Iterator[(A, A)]() {
      def hasNext: Boolean = underlying.hasNext
      def next(): (A, A) = {
        val curr   = underlying.next()
        val result = (prev, curr)
        prev = curr
        result
      }
    }
  }
}

/** Given a function which map A to an Option[B], return the first entry which maps to Some[B] or None if such an entry
  * doesn't exist.
  */
def mapFind[A, B](f: A => Option[B])(xs: Seq[A]): Option[B] = {
  xs.foldLeft(Option.empty[B])({
    case (a @ Some(_), _) => a
    case (None, b)        => f(b)
  })
}
