Given the following language

```ebnf
<t> ::= ...
    | <t> ':' <t>
    | <t> '++' <t>
    | 'head' <t>
    | 'elem' <t> <t>
    | 'length' <t>

<T> ::= ...
    | Int
    | Bool
    | [<T>]
```

Define typing rules for the terms of the language using the provided types

---

$$
\text{Tcons}\frac{\Gamma\vdash t_1 : T \quad \Gamma\vdash t_2 : [T]}{\Gamma\vdash t_1 \text{ ':' } t_2 : [T]}
$$

$$
\text{Tconcat}\frac{\Gamma\vdash t_1 : [T] \quad \Gamma\vdash t_2 : [T]}{\Gamma\vdash t_1 ++ t_2 : [T]}
$$

$$
\text{Thead}\frac{\Gamma\vdash t : [T]}{\Gamma\vdash \text{head t} : T}
$$

$$
\text{Telem}\frac{\Gamma\vdash t_1 : T \quad \Gamma\vdash t_2 : [T]}{\Gamma\vdash \text{elem } t_1 t_2 : Bool}
$$

$$
\text{Tlen}\frac{\Gamma\vdash t : [T]}{\Gamma\vdash \text{length }t : Int}
$$
