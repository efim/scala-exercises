/*
  Example of infinite streams and primes.
 */

def from(n: Int): Stream[Int] = n #:: from(n+1)

def nats = from(0)
def cuats = nats map (_*4)

def sieve(stream: Stream[Int]):Stream[Int] =
  stream.head #:: sieve(stream.tail filter (_%stream.head != 0))

def primes = sieve(from(2))

primes.take(100).toList
