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

Attempted to make a data parallel version, but could not come up with
the right monoid.

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

Couldn't math out the answer to part 2, and decided to implement an
$O(n^2)$ algorithm just to finish.

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


Day 12
------

Pre-code thoughts: Working with graphs. Interesting properties that
may be exploited: no cycles (because otherwise we wouldn't be asked to
find all cycles); graph is static; it seems that could make a derived
graph where big caves are edges and (lists/sets of) small caves are
nodes; we are working with finite regular languages. The problem again
seems quite suited for Prolog, but I'll probably just use a
backtracking monad (aka the list monad).

Haskell with backtracking via the list monad.

Unexpected: Part 2 was super fun. Started by not reading the task
description carefully and missed the points that a *single* small cave
can be visited at most twice, I thought it was *all* small caves that
could be visited twice (except for `start` and `end`).


Day 13
------

Pre-code thoughts: The task description seems to suggest that we'll be
working with sparse sets of coordinates.

Plain Haskell.


Day 14
------

Plain Haskell. Didn't have a lot of time for today's task. Thus, my
code ended up more messy than I wanted. I think I can make a monoid
for counting-maps which will make the code much nicer.

NICE TO HAVE: Data parallel version.


Day 15
------

Pre-code thoughts: Looks like a search problem, or perhaps a min-flow
networks problem. Interesting properties that may be exploited: We'll
only need to visit a place once, all costs are positive (and small). A
straightforward BFS should solve part 1, but an A* could also be fun.

Plain Haskell, simple BFS solves both part, but is rather slow.

NICE TO HAVE: Proper implementation of, say, Dijkstra's algorithm for
single-source shortest-paths.

Post event: Added functional implementations of Dijkstra's shortest
path algorithm and A* (using Manhattan distance as heuristic
function). Using either brings us under 1s for both tasks, which is
acceptable. Surprisingly, Dijkstra's algorithm is faster than
A*. Another surprise is that it seems to be slightly faster to use
`Set` (from the `containers` package) rather than a priority queue
(from the `psqueues` package) for Dijkstra algorithm.



Day 16
------

Pre-code thoughts: Looks like simple de-serialisation task.

Haskell with parser combinators and printf.


Day 17
------

Pre-code thoughts: A simulation/parameter search problem it seem. For
part1 brute force seems sufficient.

Haskell, didn't bother writing a parser as the input was trivial to
parse by hand.


Day 18
------

Pre-code thoughts: Seem like we are going to work with
trees. Observations: for part1 it seems plausible that it might not be
necessary to construct the tree to compute the magnitude (but where's
the fun in that); for addition it's unclear whether the best approach
is to do in two steps build pair then reduce, or make a smart
constructor that always return reduced pairs. Wonder if we can assume
that input is always reduced? It seems so from the task description.

Haskell, nice task. Ended up with a version that's "obviously"
correct, in the sense that each piece of code corresponds to a
specific piece of the task description. At first I tied to do both
`explodeAt` and `splits` in one pass. However, that strategy makes it
hard to give priority to `explodeAt` over `splits`, for instance there
might a split which is more to the left than an explode, in which case
we still have to take the explode over the split. Fun times.

Post event: Implemented an alternative solution based on a flat
data-structure. The `Seq` datatype in Haskell is quite nice for this
implementation. Also added an F# implementation.


Day 19
------

Solved in Haskell. Too slow.


Day 20
------

Solved in Haskell. Started by not realising that in this task it
crucial to have a border (that represents the infinite amount of
pixels that starts by being off). Thanks to Mads Obitsø Thomsen for
helping me understand this.

First time I've really used that Haskell's arrays doesn't need to be
zero-indexed. In my solution I end up with an array that's -51-indexed
:exploding_head:.


Day 21
------

Solved in Haskell using straightforward slow strategy.

Post event: Implemented an alternative solution based on move all
universes forward lockstep, and use a multi-set to keep track of all
universes. This strategy allows us to collapse universes that will
result in the same timeline.


Day 22
------

Pre-code thoughts: Looks like we'll be doing interval analysis. Thus
tries or perhaps bitsets could be useful. `IntSet` is based on
Patricia trees so it should be a good starting point. Or maybe we need
to use segment trees.

Did part by simple unfolding of cuboids to sets.

Part 2 is interesting. We end up computing the cardinality of the
union off a large set (two actually) that we don't want to construct,
we use that card(S1 ⋃ S2) = card(S1) + card(S2) - card(S1 ∩ S2). We
don't want to construct unions, because if we consider cuboids as
sets, then the union of two cuboids can generally not be denoted by a
cuboid, however the intersection of two cuboids is either empty or can
be denoted by a cuboid. Thus for each step we just need to compute
which cuboids we need to add and which overlaps (intersections) we
need to subtract.

Made two versions of part2, one using multi-sets and one using
lists. This was done to profile the benefit/overhead of using
multi-sets over lists. There is a small benefit of using multi-sets,
but not a lot when compiling the program (in ghci it's still a factor
~7).

NICE-TO-HAVE: This should be able to be done in a data parallel
manner. Could be fun to do a version in Rust.


Day 23
------

Pre-code thoughts: looks like a state space search problem.

Places:
  hallway (11): left2,left1,X,ab,X,bc,X,cd,X,right1,right2 (X's are illegal)

  rooms: a1,a2,b1,b2,c1,c2,d1,d2

Part 1: Haskell version of shortest-path-faster (I think). Runs in 20s
Part 2: Arg, the changes in the task description requires a major
refactoring. I should not have been so clever with the representation
of hallway and room places.

Part 2, solved after the 24 hour keeping-pace window. No fundamental
changes to the approach, just needed a better representation of room
places. Made part 1 run 5 times faster.

Post event: Added A* implementation, this gave a speedup of 3.


Day 24
------

Pre-code thoughts: Ah, it's symbolic execution time.

Solved with Haskell and Z3 (via the `sbv` library). Took the
opportunity to practice with `sbv`.

NICE-TO-HAVE: Clean up the code a bit, find out how to extract the
result directly from the data type returned from `sbv`.

Post event: Added a brute-force solution that explicitly keeps track
of all possible states based on input that can only be 1-9. Current
solution find the minimal model no in 20 mins, which is not as bad as
I initially expected, but now I think it should be possible to do
faster.


Day 25
------

Haskell, solved part 1 by using two sets, one for each herd. No Part 2.

