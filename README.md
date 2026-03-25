# Typechecking Example #

```
exp ::= VARIABLE | INTEGER | `true` | `false` | exp op exp
op ::= `+` | `&&` | `<` | `==`
stmt ::= type VARIABLE `=` exp `;`
program ::= stmt*

type ::= `int` | `bool`
```

