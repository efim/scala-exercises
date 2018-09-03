/*
Informallly everything with
- unit (constructor from pure value) (A => F[A])
- flatMap F[A] => (A => F[B]) => F[B]

did I remember this correctly?
'mechanism for sequencing computations'

Functors take in 'pure values' so any complications of additional context are only present in the end? beginning?
with monads method flatMap hadles intermediate complication

processing Option with function ff passed to flatMap, function ff only has to specify what happens to 'ordinary' value
sutiation of passing in None is handled by flatMap itself

map - is a sequenced computation that 'does not' introduce new monad,
flatMap - introduces new monad


 */

