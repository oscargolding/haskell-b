module Test where

data Optional a = Nada | Only a deriving (Eq, Show)

myMappend :: (Monoid a) => Optional a -> Optional a -> Optional a
myMappend Nada res = res
myMappend res Nada = res
myMappend (Only first) (Only second) = Only (first `mappend` second)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only b) = Only (a <> b)
  Nada <> Nada = Nada

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Excalamation = String

madlibbin' :: Excalamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said "
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madlibbinBetter' :: Excalamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat
    [ e,
      "! he said ",
      adv,
      " as he jumped into his car ",
      noun,
      " and drove off with his ",
      adj,
      " wife."
    ]