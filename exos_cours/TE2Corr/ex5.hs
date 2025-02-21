{-
Règle pour identifiant
<identifier> ::= ...

Règle pour les type
<type> ::= 'Int' | ...

Déclaration de type
<type-decl> ::= 'type' <identifier>+ '=' <type>
                | 'type' <identifier> <identifier>* '=' <type>
                | 'type' <identifier> {<identifier>} '=' <type>
il acceptait toutes ces possibilités mais préfère celle avec {}
-}

-- Abstract syntax tree
-- Pour les types
data Type = TInt -- etc
-- Déclaration de type
data TypeDecl = TypeAlias String [String] Type