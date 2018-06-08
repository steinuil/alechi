The list of reserved words in C11:
```
auto break case char const continue default do double else enum extern float
for goto if inline int long register restrict return short signed sizeof static
struct switch typedef union unsigned void volatile while _Alignas _Alignof
_Atomic _Bool _Complex _Generic _Imaginary _Noreturn _Static_assert
_Thread_local
```

Here's the grammar of the s-expressions representation:
```
declaration ::=
  | (include <string>)
  | (include-rel <string>)

  | (typedef <ident>
      <type-expr>)

  | (struct <ident>
      (<ident> <type-expr>) ...)
  | (union <ident>
      (<ident> <type-expr>) ...)

  | (enum <ident>
      <enum-item> ...)

  | (decl <ident> <type-expr>
      <expression>?)

  | (proc <ident> ((<ident> <type-expr>) ...) <type-expr>
      <statement> ...)


constant ::=
  | ('string <string>)
  | ('char <string>)
  | ('int <string>)
  | ('float <string>)
  | ('enum <string>)


enum-item ::=
  | <ident>
  | (<ident> <constant>)


type-expr ::=
  | <ident>


expression ::=
  | <constant>
  | <ident>
  | (<ident> <expression> ...)


statement ::=
  | (decl <ident> <type-expr>
     <expression>?)
  | (set <ident> <expression>)
  | (ignore <expression>)
  | (label <ident>)
  | (goto <ident>)
  | (if <expression>
      <statement>
      <statement>?)
  | (return <expression>)

```
