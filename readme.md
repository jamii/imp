# motivation

want to easily write and compose stateful gui tools over relational database

datalog is nice because

* disorderly by default - doesn't introduce unnecessary ordering
  * all state is easily reproducible / queryable
  * minimum fussing with loops etc
  * data-flow is easier to extend than control-flow
  * have efficient algos for incremental maintenance, so don't have to worry about db->ui statefully
* first-order by default
  * minimum fussing with data-structures
* meshes well with relational database model

datalog is gross because

* hard to abstract over repeated patterns
* pointful = verbose
* aggregation doesn't mix well with disorderly model
* stratification doesn't compose well

prolog/mercury/kanren et al extend datalog by adding data-structures and unification, but this breaks the disorderliness eg typical prolog tutorial is using recursion to do linear search over linked list - not very relational.

rai trying to extend datalog by adding macros and meta-programming. dubious of how well-grounded this ends up being. macro system alone essentially contains a strict lambda calculus.

so let's start with a strict lambda calculus, where expressions evaluate to first-order relations, and see how much of the niceness we can get while still abstracting via this more well understood model.

difficult points are modules (keeping data globally named), recursion (stratification doesn't compose well), staging (when is a dataflow a dataflow?).

perhaps something like - dataflow (containing recursion / fix points) must be installed with a name to be evalled.

# design constraints

want to avoid having to materialize intermediate sets, and especially avoid having to use them as keys.

possibly sufficient restrictions:

* every higher-order function has a finite number of specializations
  * could be enforced by type system
  * could be implemented by jit specialization
* every application of a higher-order function can be specialized into a first-order function
  * ie the set closed over needs to be expressible as a function of scalars

also need to be careful that the rewrite doesn't balloon the amount of code. that kind of rules out just inlining.

every value is namable by (location of expression) x (free scalar variables)

# first-order

rewrite to get static dataflow graph:

* (Optional) if let is only used once, inline it
* Lift all lets up to top-level `a -> let b = ... in ...` => `let b = a -> ... in ...`
* Lift lambdas up to lets `a | (b -> ...)` => `b -> a b | ...`
* (Optional) infer arity and lambda-ize remaining relations

__evaluation is implemented, rewrite is not__

# higher-order via staging

__runtime higher-order is implemented, need to switch to compile-time__

naive macros-as-inlining can duplicate work and confuse evaluation order. use specialization instead - like how rust deals with closures.

* antiquoting a quote just removes it - `${a}` => `a`
* antiquoting a function evaluates that function on a quote - `$(a -> f a) b` => `$(eval {f {b}})`
* anything else is an error

i think we can just interleave specialization and the lowering rewrite above until fixpoint, and like rust just bail if it takes a bajillion steps.

# recursion

__not implemented__

???

arbitrary recursion doesn't compose well because of stratification

maybe want to have an explicit loop operator like DD?
