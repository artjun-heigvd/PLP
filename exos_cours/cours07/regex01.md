Write a regular expression for the language with Sigma = {0, 1}

{w | w starts and ends with the same character}


---

```
regex = (1((0|1)*1)?) | (0((0|1)*0)?) 
     ou (1[0-1]*1) | (0[0-1]*0) | 1 | 0 
```

```mermaid
flowchart LR
    S((S))
    A((A))
    B(((B)))
    C((C))
    D((D))
    E(((E)))
    F((F))

    S ----> A
    A -- 0 --> B
    B -- 0 --> B
    B -- 1 --> D
    D -- 1 --> D
    D -- 0 --> B
    E -- 0 --> C
    C -- 0 --> C
    S ----> F
    F -- 1 --> E
    E -- 1 --> E
    C -- 1 --> E
```
