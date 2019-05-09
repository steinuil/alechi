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

- Use modular implicits for allocating and deallocating structures and memory
  with polymorphic alloc and free functions. C++ has something similar with
  allocators, Zig passes allocators to functions that need them.

- Optionally emit clang module maps for faster compilation.

- Keyword for explicit tail calls (become, jump...). (how would this even work
  in C)

- Different types of strings prefixed by a letter (e.g. b"foo"), maybe even
  user-definable.


## Links
Some stuff to read and research.

### Clang modules
* https://clang.llvm.org/docs/Modules.html

### A theory of type qualifiers
* https://dl.acm.org/citation.cfm?doid=301618.301665

### Type Qualifiers as Composable Language Extensions
* https://dl.acm.org/citation.cfm?id=3136055
* https://www.youtube.com/watch?v=jFsnN4dFRZA

### Compiling SML to C
* http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.43.7510

### Resource Polymorphism: adding destructors and move semantics to OCaml
* https://arxiv.org/abs/1803.02796

### Practical Affine Types
* http://users.eecs.northwestern.edu/~jesse/pubs/alms/

### All the languages together
* https://www.youtube.com/watch?v=3yVc5t-g-VU


# Stuff
The name is the italian spelling of the japanese nickname (Aleki) of Alexandrite
from Houseki no Kuni (Land of the Lustrous). I have no excuse to name the
language like this other than I like the character. And it's pretty googleable.
