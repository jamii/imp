Start with a strict lambda calculus where every expression returns a relation or a closure.

* 'Booleans'
  * the empty relation - `nothing`
  * the one-row zero-column relation - `something`

* Scalars
  * numbers - `42.7`
  * strings - `"foo"`

* Union (commutative, associative, idempotent)
  * `nothing | a` => `a`

* Product (associative)
  * `nothing x a` => `nothing`
  * `a x nothing` => `nothing`
  * `something x a` => `a`
  * `a x something` => `a`
  * `(a | b) x z` => `(a x z) | (b x z)`
  * `a x (y | z)` => `(a x y) | (a x z)`

* Application (commutative, not associative!)
  * `"foo" "foo"` => `something`
  * `"foo" "bar"` => `nothing`
  * `f nothing` => `nothing`
  * `f something` => `f`
  * `f (a | b)` => `(f a) | (f b)`
  * `f (a x b)` => `f a b`

* Abstraction
  * `(a -> ...) "foo"` => `let a = "foo" in ...`
  * (ie this rule only applies to scalars like "foo")

* Solving
  * `[f]` returns a finite relation which behaves exactly like `f` in application
  * or returns an error __at compile time__ if the compiler can't rewrite `f`
  * eg `[a -> a 1 | a 2]` => `(1 | 2)`
  * eg `[a -> foo a | bar a]` => `foo | bar`
  * eg `[a -> a > 1]` => `error`
  * eg `[a c -> exists(b -> parent a b & parent b c)]` => ...
  * this provides declarative relation comprehensions whilst limiting failure to a lexical region

* Boxing
  * `{expr}` is a scalar containing a thunk which evaluates to `expr`
  * (two thunks are equal when they are defined at the same location and capture equal values)
  * (this is the only non-monotonic operation!)
  * various built-in functions expect boxes as arguments eg `equals`, `reduce`
  * various operators implicitly box their arguments eg `a = b` => `equals {a} {b}`, `!a` => `{a} != nothing`

* Unboxing
  * only allowed inside another box
  * `{ ${a} }` => `{ a }`
  * `{ $(f) b }` => `{ $(f {b}) }`
  * (TODO we can probably evaluate the inner function at box-creation time - `{b}` will be an open quotation, but it can't escape this scope so I don't _think_ it's observable to the programmer)

Apply these rules until you get to a union of products of scalars (the canonical form for a relation).

If the result is a single box, open it and continue.

If you terminate without reaching the canonical form, it's an error (eg applying a function to another function).

---

Eg `let name = (1 x "one" | 2 x "two") in name 1`
=> `(1 x "one" | 2 x "two") 1`
=> `((1 x "one") 1) | ((2 x "two") 1)`
=> `(1 (1 x "one") | (1 (2 x "two"))`
=> `(1 1 "one") | (1 2 "two")`
=> `(something "one") | (nothing "two")`
=> `"one" | nothing`
=> `"one"`

---

The idea of the boxes is that you can't ever open them, only return them. If the database is also passed to you as boxed values, then your metaprogram can't depend on the values in the database. So instead of evaluating the boxes immediately at creation we can instead compile them to differential dataflows that will continue to evaluate them as the database changes.

```
let people = db "people" in
let count = db "count" in
let department = db "department" in
let group_by = (things reducer key -> {
  [k ->
    let group = [t -> ($things t) & ($key t k)] in
    $reducer group
  ]
}) in
{ $group_by $people $count $department }
```

Effectively, we get to reuse the first-order language as a higher-order meta-language to describe first-order dataflows.

---

TODO

Think about recursion

Modules

---

If we allow asking limited questions about boxes (eg what is the return type) we can also get some form of adhoc polymorphism.

```
let add_impls = (integer x integer x {a b -> a + b} | string x string x {a b -> concat a b}) in
let add = \ a b -> { $(add_impls typeof(a) typeof(b)) $a $b } in
{ $add "foo" bar" }

=>

{ (a b -> concat a b) "foo" "bar" }
```
