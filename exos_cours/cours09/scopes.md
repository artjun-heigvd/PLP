Given the following snippet

```hs
import Prelude (elem, reverse)

unique :: Eq a => [a] -> [a]
unique xs = unique' xs []
    where
        unique' [] xs' = reverse xs'
        unique' (x:xs) xs'
            | x `elem` xs' = unique' xs xs'
            | otherwise    = unique' xs (x:xs')
```

Sketch the scopes created by this snippet

---

```
GlobalScope (
    unique,
    elem,
    reverse

    FunctionScope (
        xs

        LocalScope(
            unique'

            FunctionScope (
                xs'
            )

            FunctionScope (
                x,
                xs,
                xs'
            )
        )
    )
)
```

Le `LocalScope` peut être omis mais est utile pour représenter le `where` du Haskell.