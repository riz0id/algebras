# Well-founded recursion constraints via F-algebras in Haskell

## Overview

Algebras is a collection type classes providing the structure for F-algebras and common operations on inductive types derived from those structures as well as a free isomorphism for converting between inductive types. Algebras values generality but not at the expense of efficency - only operations whose asymptotic complexity could not be smaller in a specific case are provided. For example the general implementation for `drop` of O(n) cannot ergonomically compete with the O(1) in `Data.Seq.drop` thus it is not provided; however, `dropWhile` is necessarily O(n) so we can derive it from an instance of `FCoalgebra`.

## Application

Algebras is suitable for use case where we care about being collection indiscriminate. For example a combinatorial parser can be implemented entirely from a `FCoalgebra`. Another minor benefit is that explicit implementations of list-like operations which would typically collide are now aggergated under a single interface.

## Usage

F-algebras model constructors for inductive types, so for example we can define lists as an F-algebra by

```Haskell
instance FAlgebra [a] where
  -- The type of elements in our collection.
  type FElem [a] = (a, [a])

  -- ^ How we append an element to our collection.
  fcons (x, xs) = x : xs

instance FCoalgebra [a] where
  -- The result of a destruction.
  type FCoelem [a] = Maybe (a, [a])

  -- How we destruct an element of [a].
  funcons []       = Nothing
  funcons (x : xs) = Just (x, xs)
```

similar instaces exists for:

* [Text](https://hackage.haskell.org/package/text-1.2.4.0)
* [Seq](https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Sequence.html#t:Seq)
* [ByteSring](https://hackage.haskell.org/package/bytestring-0.11.0.0/docs/Data-ByteString.html)
* [DList](https://hackage.haskell.org/package/dlist-1.0/docs/Data-DList.html#t:DList)
* etc...

## Contributing

Issue and pull requests are welcome so long as the changes adhere to the guiding princples of the project mentioned in the overview. I could be convinced of extending the library with things like `drop` or `take` if comes packaged in a way that does not sacrifice type inference, ergonomics, or efficency but I have not found any great canidates for such a thing.

### Future Work

If you'd like to contribute to algebras here is a current list of ways the library could be extended.
* Add notionally distinct instances for algebras using [`Reverse`](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/Data-Functor-Reverse.html) to implement snoc, last, and other functions operating at the end of lists.
* Add generics to automatically derive F-algebras and F-coalgebras if possible.
