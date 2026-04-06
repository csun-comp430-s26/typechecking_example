# Typechecking Example #

```
structdec ::= `struct` STRUCT_NAME `{` (type STRUCT_FIELD `;`)* `}`

fielddec ::= STRUCT_FIELD `:` exp
fielddecs ::= [fielddec (`,` fielddec)*]
exp ::= VARIABLE | INTEGER | `true` | `false` | exp op exp |
        exp `.` STRUCT_FIELD | `new` STRUCT_NAME `{` fielddecs `}`
        
op ::= `+` | `&&` | `<` | `==`
stmt ::= type VARIABLE `=` exp `;`
program ::= structdec* stmt*

type ::= `int` | `bool` | STRUCT_NAME
```

