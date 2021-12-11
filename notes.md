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

Dang, undergrad math is far away. Remembered that median could be used
for part 1 (a similar question was on my exam back then, something
something about linearity and optimality).

Couldn't math out the answer to part 2, and decided to make $O(n^2)$
algorithm just to finish.

NICE TO HAVE: Clean up code.


Day 8
-----

The plan was to make a version with Prolog, or a backtracking monad in
Haskell. Alas, my energy level was low, so I just made a simpleminded
Haskell version. It's data parallel though.

Unexpected: Couldn't remember how to read a text file in Prolog

NICE TO HAVE: Make a Prolog version.


Day 9
-----

Haskell. OK flood-fill algorithm, would have like too use either
union-find for the flooding or have used the standard data parallel
basin algorithm. Maybe in the future.

Part1 is morally data parallel.

Part2: There are some tasty constrains in the problem spec: heights are
constrained to 0-8, all locations (except walls) are part of exactly
one basin, however I didn't need to exploit any of them to get OK
running time. I suspect that it is possible to make an elegant
one-pass data parallel algorithm because of these constrains.

Unexpected: `Vector` was clunky to use. I started out by trying to
make my own 2D view of 1D vector, but quickly realised that is wasn't
worth it for this task. I missed `Array2` from SML.

NICE TO HAVE: Clean up code. Implement with union-find in Rust or
Haskell. Implement SML version.


Day 10
------

Was in the mood for SML. Nothing fancy.

Unexpected: Slightly disappointed about the part2 task description. It
would have been more interesting to find minimal corrections to *all*
lines and not just the incomplete lines. Although the *minimal*
requirement might go too far into "Hmm, that's interesting" territory.

NICE TO HAVE: Maybe a data parallel version that uses the stack
monoid. Solution to own my idea for part2, just to see how difficult
it is (I think the obvious algorithm is optimal, but I haven't checked.)


Day 11
------

Pre-code thoughts: Part 1 looks like it's naturally solved by using
mutation,and the is some flood-fill like at day 9. Should I try to
reuse my code from day 9, or should I switch to use an imperative 2D
array in Haskell, if so I might as well switch to SML, F#, or
Rust. Maybe use `massiv`.

Decided to use F# to work a bit with `Array2D` and `Seq`.

Unexpected: `Array2D` is not a `Seq`, this made everything rather
clunky. I think I spend the majority of my time being surprised about
missing library functions for `Array2D` and fighting the rather verbose
generator/sequence syntax.

NICE TO HAVE: Haskell or Rust version for comparison. Maybe use active
patterns for `flood`.
