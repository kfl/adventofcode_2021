Notes and Reflections from AoC 2021
===================================

**Spoiler Warning: The following contains spoilers for the AoC tasks,
so stop reading if you haven't solved the tasks yet.**


Day 1
-----

Main trick is to figure how to work with "windows" in your language of
choice. Both F# and Rust have nice library support for this.

Straightforward to make data parallel, implemented actual parallel
Rust version with Rayon.

Unexpected: The parsing code in Rust ended up much less elegant than I
wanted.

NICE TO HAVE: Make a Haskell version.


Day 2
-----

Attempted to make a data parallel version, but could come up with the
right monoid.

Wrote some nice QuickCheck.


Day 3
-----

Both an SML and a Haskell version.

Started with SML for part1 and then switched to Haskell as I didn't
want to implement a bunch of utility functions again, and I predicted
that Haskell would have what I need. Turns out I was wrong, I ended up
implementing `parseBin` for the umpteen time.

After getting my stars, I added an SML version for part2. It turned
out as nice if not nicer than the Haskell version.


Day 4
-----

Haskell. Used `scanl`, should be data parallel.

NICE TO HAVE: Make a Rust version.

Day 5
-----

Data parallel Haskell.


Day 6
-----

Haskell. Uses vectors. Since step is only using `backpermute` and
`accumulate` it can be written as a matrix multiplication.

NICE TO HAVE: Make a Rust version where `matrix_power` is a `const`
function and permutation matrices for day 80 and 256 are computed at
compile time.


Day 7
-----

Dang, undergrad math is far away. Remembered that media could be used
for part 1 (a similar question was on my exam back then, something
something about linearity and optimality).

Couldn't math out the answer to part 2, and decided to make O(n^2)
algorithm just to finish.

NICE TO HAVE: Clean up code.

Day 8
-----

The plan was to make a version with Prolog, or a backtracking monad in
Haskell. Alas, my energy level was low, so I just made a simpleminded
Haskell version. It's data parallel though.

Unexpected: Couldn't remember how to read a text file in Prolog

NICE TO HAVE: Make a Prolog version.