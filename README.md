# Alechi
Alechi will be a language that compiles to C. Its goal is to provide roughly
all of C's features and some more, with a better type system and some syntactic
sugar.

## Some ideas
- A module system similar to ML's, with separate interface and implementation
  files (yes it will have headers). Bindings private to a module should
  probably still be callable, but they should output a warning.

- ML functors? (so that a program could e.g. define its custom allocator for
  the functions in the standard library).

- No dependency on the C standard library or any other C library. Output
  pure C with no GCC extensions.

- External C functions would not be directly callable, but would have to be
  declared in a module without implementation.

- Syntax should be a mix of C, ML, ReasonML, Jai. Curly braces, semicolons and
  snake\_case everywhere. Modules and ADTs PascalCased.

- Extend the ML type system with type classes, either similar to OCaml (modular
  implicits) or Ur/Web (normal values). The language would make heavy use of
  these for e.g. testing equality, arithmetic, iterating, casts...


## To research
https://dl.acm.org/citation.cfm?doid=301618.301665

Type Qualifiers as Composable Language Extensions -
https://dl.acm.org/citation.cfm?id=3136055 -
https://www.youtube.com/watch?v=jFsnN4dFRZA


# Stuff
The name is the italian spelling of the japanese nickname (Aleki) of Alexandrite
from Houseki no Kuni (Land of the Lustrous). I have no excuse to name the
language like this other than I like the character. And it's pretty googleable.
