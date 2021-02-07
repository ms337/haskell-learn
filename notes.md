# Notes

## Syntax (highlights)
Function application has the highest precedence, so foo 8 9 + bar 2 4 is the same as (foo 8 9) + (bar 9 3)

Ranges:
	- numeric
	- steps by [1, 4, ... 90]
	- Avoid floats in ranges

Infinite Lists
	- [1, 3, ..]
	- Lazy evaluated

	Cycle

List Comprehensions
	[ | <- , , , ]



## Some Things to Keep in Mind

The first step in writing a Haskell program is usually to write down all the types. Because Haskell’s type system is so expressive, this is a non-trivial design step and is an immense help in clarifying one’s thinking about the program.

Haskell is very good at abstraction: features like parametric polymorphism, higher-order functions, and type classes all aid in the fight against repetition.



## Types vs. Typeclasses

### Types

### Typeclass are like an interface that define some behaviour

    A Typeclass is not the same thing as a Type

    For Prefix functions, passing them around, calling them as a prefix function requires us to surround themselves with parantheses

    => symbol
    Everything before the => is called a class constraint

    (==) :: (Eq a) => a -> a -> Bool

    Eq class provides an interface for testing for equality. All standard Haskell types except for IO and functions are a part of the Eq typeclass

    Example: :t elem
                elem :: (Eq a) => a -> [a] -> Bool

#### Some basic typeclasses

    1. Eq
    2. Ord: types that have an ordering
        Ord vs Ordering: Ordering is a type (not a typeclass) that can GT, LT or EQ.
        To be of type Ord, a type must be in Eq
    3. Show: members of show can be presented as strings
        - Type to String
        - show function
    4. Read: String to Type

### Type Annotations

    read "4"
    returns an error because it does not know what we want

    if we do read "4" + 5
    returns 9 because it can infer what we want

    To fix the eror mentioned before the above, we use type annotations.

## Enumeration Types

## Algebraic Types

## Type Constructors vs. Data Constructors

        data Person = Person String String Int
        type constructor x = data constructor x _ _ _

## Pattern Matching

## Recursion Patterns

"Experienced Haskell programmers hardly ever write recursive functions!

How is this possible? The key is to notice that although recursive functions can theoretically do pretty much anything, in practice there are certain common patterns that come up over and over again. By abstracting out these patterns into library functions, programmers can leave the low-level details of actually doing recursion to these functions, and think about problems at a higher level—that’s the goal of wholemeal programming.

1. Map
2. Filter
3. Fold

## Polymorphism

### Polymorphic Datatypes


#### Maybe Type
data Maybe a = Nothing | Just a -- similar to Optionals?

### Polymorphic Functions
One important thing to remember about polymorphic functions is that the caller gets to pick the types. When you write a polymorphic function, it must work for every possible input type. This—together with the fact that Haskell has no way to directly make make decisions based on what type something is—has some interesting implications which we’ll explore later.


## Total and Partial functions

