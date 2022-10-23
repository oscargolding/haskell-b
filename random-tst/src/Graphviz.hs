{-# LANGUAGE QuasiQuotes #-}

module Graphviz where

import Control.Applicative (Alternative (empty, many, some, (<|>)), optional)
import Data.Char (isAlpha, isDigit, toLower, toUpper)
import Data.Foldable (find)
import qualified Data.Maybe
import Test.Hspec (xcontext)
import Text.Parsec.Token (GenLanguageDef (caseSensitive))
import Text.RawString.QQ (r)
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Parsing (skipMany, try),
    Result (Success),
    anyChar,
    count,
    integer,
    letter,
    manyTill,
    natural,
    noneOf,
    oneOf,
    parseString,
    satisfy,
    string,
  )

-- Example data
exampleDoubleQuotedString :: String
exampleDoubleQuotedString =
  [r|"hey there lolol
baby
I am a baby wahhhh" + 
" You are such a baby" +   
"Who is a good little boy?"|]

simpleID :: String
simpleID = [r|"eg id"|]

egAttrList :: String
egAttrList = [r|[ "cat"="hat" ; "cat"="dog" ]|]

complexAttrList :: String
complexAttrList = [r|[ "cat"="hat" ; "black"="dog" ] ["hey"="hi"]|]

cStyleComment :: String
cStyleComment =
  [r|
/*
look at this
*/|]

nodeStmt :: String
nodeStmt =
  [r|"eg id" : "port id" : ne [ "cat"="hat" ; "cat"="dog" ] 
// simple comment is here 
// gee whiz
/*
c style comment
*\
[ "hey"="hi" ]
|]

exampleStmtList :: String
exampleStmtList =
  [r|{ "eg id" : "port id" : ne [ "cat"="hat" ; "cat"="dog"] ;
// Another statement
"new stmt" : "new id" : se [ "oh"="my" ; "try"="now" ]
}
|]

simpleStatement :: String
simpleStatement =
  [r|"new stmt" : "new id" : se [ "oh"="my" ; "try"="now" ]
|]

basicGraphType :: String
basicGraphType =
  [r|DIGRAPH "my-graph" {
  "eg id" : "port id" : ne [ "cat"="hat" ; "cat"="dog"] ;
  "new stmt" : "new id" : se [ "oh"="my" ; "try"="now" ]
}
|]

strictGraphType :: String
strictGraphType =
  [r|STRICT DIGRAPH "my-graph" {
  "eg id" : "port id" : ne [ "cat"="hat" ; "cat"="dog"] ;
  "new stmt" : "new id" : se [ "oh"="my" ; "try"="now" ] ;
  subgraph "sub id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
  } ;
  "this"="report" ;
  GRAPH [ "oh"="my" ; "try"="now" ]
  subgraph "sub id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
  }->c->d->c->d->subgraph "new id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
  } [ "this"="isAlist" ; "wow"="another example" ]
}
|]

stmtGraph :: String
stmtGraph =
  [r|STRICT DIGRAPH "my-graph" {
  "this"="report" ;
}
|]

attributeStmt :: String
attributeStmt = [r|GRAPH [ "oh"="my" ; "try"="now" ] |]

subGraphString :: String
subGraphString =
  [r|subgraph "sub id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
  }|]

exampleRHS :: String
exampleRHS = [r|->c->d->e|]

exampleRHSWSubgraph :: String
exampleRHSWSubgraph =
  [r|->c-> subgraph "sub id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
  }->e
|]

exampleEdgeStatement :: String
exampleEdgeStatement =
  [r|a->b->c|]

edgeStmtSubgraph :: String
edgeStmtSubgraph =
  [r|subgraph "sub id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
}->c->d->subgraph "new id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
}
|]

edgeStmtSubgraphAttList :: String
edgeStmtSubgraphAttList =
  [r|subgraph "sub id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
}->c->d->subgraph "new id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
} [ "this"="isAlist" ; "wow"="another example" ]
|]

aListEg :: String
aListEg =
  [r|--b--c--d--subgraph "new id" {
    "sub stmt" : "sub id" : se [ "oh"="my" ; "try"="now" ] ;
} [ "this"="isAlist" ; "wow"="another example" ]|]

-- Types
type GraphVizID = String

data CompassPt = N | NE | E | SE | S | SW | W | NW | C | UNDER
  deriving (Eq, Show)

data Port = CPort CompassPt | IDPort GraphVizID CompassPt deriving (Eq, Show)

data NodeID = SNodeID GraphVizID | CNodeID GraphVizID Port deriving (Eq, Show)

data Attribute = AttributePair GraphVizID GraphVizID deriving (Eq, Show)

type AList = [Attribute]

type AttrList = [AList]

data NodeStmt = SNodeStmt NodeID | CNodeStmt NodeID AttrList deriving (Eq, Show)

data AttStmtTypes = GraphA | NodeA | EdgeA deriving (Eq, Show)

data AttributeStatement
  = AttributeStatement
      AttStmtTypes
      AttrList
  deriving (Eq, Show)

data EdgeOp = DArrow | UArrow deriving (Eq, Show)

data EdgeRHS = ENode EdgeOp NodeID | ESub EdgeOp Subgraph deriving (Eq, Show)

type EdgeList = [EdgeRHS]

type EdgeListTup = [(EdgeRHS, EdgeRHS)]

data BaseEdgeStmt
  = BasicEStmt EdgeList
  | AttrEStmt EdgeList AttrList
  deriving (Eq, Show)

data EdgeStmt
  = NEStmt NodeID BaseEdgeStmt
  | SStmt Subgraph BaseEdgeStmt
  deriving (Eq, Show)

data Stmt
  = Node NodeStmt
  | Subgraph' Subgraph
  | Attribute' Attribute
  | AttributeStatement' AttributeStatement
  | EdgeStmt' EdgeStmt
  deriving (Eq, Show)

type StmtList = [Stmt]

data Subgraph = SubgraphID GraphVizID StmtList | Subgraph StmtList
  deriving (Eq, Show)

data GraphType = Graph | Digraph deriving (Eq, Show)

data BasicGraphBase = GraphBase' GraphType StmtList deriving (Eq, Show)

data GraphBase
  = BGraphBaseID BasicGraphBase GraphVizID
  | BGraphBase BasicGraphBase
  deriving (Eq, Show)

data Graph = StrictGraph GraphBase | NonStrictGraph GraphBase
  deriving
    (Eq, Show)

--

parseAlphaIDChar :: Parser Char
parseAlphaIDChar = do
  satisfy (\x -> isAlpha x || (x == '_') || isDigit x)

parseAlphaID :: Parser GraphVizID
parseAlphaID = do
  firstChar <- satisfy (\x -> isAlpha x || (x == '_'))
  restOfString <- many parseAlphaIDChar
  return (firstChar : restOfString)

parserLowDot :: Parser String
parserLowDot = do
  x <- char '.'
  digitString <- some $ satisfy isDigit
  return (x : digitString)

parseHighDot :: Parser String
parseHighDot = do
  some $ satisfy isDigit

parseHighLowDot :: Parser String
parseHighLowDot = do
  high <- parseHighDot
  x <- char '.'
  low <- some $ satisfy isDigit
  return (high ++ "." ++ low)

totalDotParser :: Parser String
totalDotParser = do
  try parserLowDot <|> try parseHighLowDot <|> parseHighLowDot

optionalDashDot :: Parser String
optionalDashDot = do
  x <- char '-'
  final <- totalDotParser
  return (x : final)

dotNumeralParser :: Parser String
dotNumeralParser = do
  optionalDashDot <|> totalDotParser

normalString :: Parser String
normalString = some $ noneOf "\\\""

quotedString :: Parser String
quotedString = do
  string "\\\""
  res <- normalString
  string "\\\""
  return $ "\"" <> res <> "\""

escapedString :: Parser String
escapedString = do
  try normalString <|> quotedString

stringIDParser :: Parser String
stringIDParser = do
  char '"'
  x <- many escapedString
  char '"'
  return $ "\"" <> concat x <> "\""

-- Skippers
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipSimpleComment :: Parser ()
skipSimpleComment =
  skipMany
    ( do
        _ <- string "//"
        skipMany (noneOf "\n")
        skipEOL
    )

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany
    ( try
        ( do
            _ <- string "/*"
            manyTill anyChar (string "*\\")
            skipEOL
        )
        <|> try
          ( do
              _ <- string "//"
              skipMany (noneOf "\n")
              skipEOL
          )
        <|> ( do
                _ <- char ' ' <|> char '\n'
                return ()
            )
    )

--

idAdderParser :: Parser String
idAdderParser = do
  skipWhitespace
  _ <- char '+'
  skipWhitespace
  tail . init <$> stringIDParser

finalAdderParser :: Parser String
finalAdderParser = do
  first <- stringIDParser
  x <- some idAdderParser
  return $ init first ++ concat (x ++ ["\""])

finalStringIDParser :: Parser String
finalStringIDParser = do
  try finalAdderParser <|> stringIDParser

htmlParser :: Parser String
htmlParser = do
  l <- char '<'
  m <- many $ noneOf ">"
  r <- char '>'
  if checkHTML m
    then return $ "<" <> m <> ">"
    else error "cannot contain newline or plus symbol"
  where
    checkHTML :: String -> Bool
    checkHTML string = foldr checker True string
      where
        checker :: Char -> Bool -> Bool
        checker _ False = False
        checker char True = not (char /= '+' && char /= '\n')

-- Parse the graphviz id
parseGraphVizID :: Parser GraphVizID
parseGraphVizID = do
  try parseAlphaID
    <|> try dotNumeralParser
    <|> try finalStringIDParser
    <|> htmlParser

parseCompassPt :: Parser CompassPt
parseCompassPt = do
  skipWhitespace
  x <- anyChar
  skipWhitespace
  case x of
    'n' -> return N
    'e' -> return E
    's' -> return S
    'c' -> return C
    '_' -> return UNDER
    _ -> fail "No single character compass point"

parseLargerCompassPt :: Parser CompassPt
parseLargerCompassPt = do
  skipWhitespace
  x <- anyChar
  y <- anyChar
  skipWhitespace
  case (x, y) of
    ('n', 'e') -> return NE
    ('s', 'e') -> return SE
    ('s', 'w') -> return SW
    ('n', 'w') -> return NW
    _ -> fail "Not a compass point."

-- Parse the compass point
parseTotalCompassPt :: Parser CompassPt
parseTotalCompassPt = do
  try parseLargerCompassPt <|> parseCompassPt

portParserFirst :: Parser Port
portParserFirst = do
  skipWhitespace
  _ <- char ':'
  skipWhitespace
  id <- parseGraphVizID
  skipWhitespace
  _ <- char ':'
  skipWhitespace
  IDPort id <$> parseTotalCompassPt

portParserSecond :: Parser Port
portParserSecond = do
  skipWhitespace
  _ <- char ':'
  skipWhitespace
  CPort <$> parseTotalCompassPt

parsePort :: Parser Port
parsePort = do
  try portParserFirst <|> portParserSecond

parseNodeID :: Parser NodeID
parseNodeID = do
  try parseDetailed <|> parseSimple
  where
    parseDetailed :: Parser NodeID
    parseDetailed = do
      id <- parseGraphVizID
      CNodeID id <$> parsePort

    parseSimple :: Parser NodeID
    parseSimple = do
      SNodeID <$> parseGraphVizID

--- Attribute parser
parseAttribute :: Parser Attribute
parseAttribute = do
  x <- parseGraphVizID
  skipWhitespace
  _ <- char '='
  skipWhitespace
  AttributePair x <$> parseGraphVizID

attributeStmtParser :: Parser Stmt
attributeStmtParser = do
  Attribute' <$> parseAttribute

parseAttributeSecond :: Parser Attribute
parseAttributeSecond = do
  x <- parseGraphVizID
  skipWhitespace
  _ <- char '='
  skipWhitespace
  y <- parseGraphVizID
  skipWhitespace
  _ <- satisfy (\x -> (x == ';') || (x == ','))
  skipWhitespace
  return $ AttributePair x y

attributeParser :: Parser Attribute
attributeParser = do
  try parseAttributeSecond <|> parseAttribute

aListParser :: Parser AList
aListParser = do
  some attributeParser

attrListParser :: Parser AList
attrListParser = do
  skipWhitespace
  _ <- char '['
  skipWhitespace
  list <- aListParser
  skipWhitespace
  _ <- char ']'
  skipWhitespace
  return list

finalAttrListParser :: Parser AttrList
finalAttrListParser = do
  some attrListParser

attributeStatementParser :: String -> AttStmtTypes -> Parser Stmt
attributeStatementParser str typ = do
  skipWhitespace
  x <- caseInsensitiveString str
  skipWhitespace
  AttributeStatement' . AttributeStatement typ <$> finalAttrListParser

attributeAllParser :: Parser Stmt
attributeAllParser = do
  try (attributeStatementParser "graph" GraphA)
    <|> try (attributeStatementParser "node" NodeA)
    <|> try (attributeStatementParser "edge" EdgeA)

parseNodeStmt :: Parser NodeStmt
parseNodeStmt = do
  try complexStatement <|> simpleStatement
  where
    simpleStatement :: Parser NodeStmt
    simpleStatement = do
      SNodeStmt <$> parseNodeID

    complexStatement :: Parser NodeStmt
    complexStatement = do
      x <- parseNodeID
      CNodeStmt x <$> finalAttrListParser

parseFinalNodeStmt :: Parser Stmt
parseFinalNodeStmt = do
  Node <$> parseNodeStmt

--- Statement Parsers

------------------- Actual Statement Semantics
parseStatementBasic :: Parser Stmt
parseStatementBasic = do
  try attributeStmtParser
    <|> try parseEdgeStmt
    <|> try attributeAllParser
    <|> try subgraphParser
    <|> try parseFinalNodeStmt

parseStatementComplex :: Parser Stmt
parseStatementComplex = do
  x <-
    try attributeStmtParser
      <|> try parseEdgeStmt
      <|> try attributeAllParser
      <|> try subgraphParser
      <|> try parseFinalNodeStmt
  skipWhitespace
  _ <- char ';'
  skipWhitespace
  return x

-------------------

statementParser :: Parser Stmt
statementParser =
  do
    try parseStatementComplex
    <|> parseStatementBasic

statementListParser :: Parser StmtList
statementListParser = do
  skipWhitespace
  _ <- char '{'
  skipWhitespace
  res <- many statementParser
  skipWhitespace
  _ <- char '}'
  skipWhitespace
  return res

---

--- Subgraph Parser
subgraphWithIdParser :: Parser Subgraph
subgraphWithIdParser = do
  skipWhitespace
  _ <- caseInsensitiveString "subgraph"
  skipWhitespace
  x <- parseGraphVizID
  SubgraphID x <$> statementListParser

subgraphNoIdParser :: Parser Subgraph
subgraphNoIdParser = do
  skipWhitespace
  _ <- caseInsensitiveString "subgraph"
  skipWhitespace
  Subgraph <$> statementListParser

subgraphBasic :: Parser Subgraph
subgraphBasic = do
  Subgraph <$> statementListParser

getSubgraph :: Parser Subgraph
getSubgraph = do
  try subgraphWithIdParser <|> try subgraphNoIdParser <|> subgraphBasic

subgraphParser :: Parser Stmt
subgraphParser = do
  Subgraph' <$> getSubgraph

---

--- Edge Parser
eopParserU :: Parser EdgeOp
eopParserU = do
  _ <- string "--"
  return UArrow

eopParserD :: Parser EdgeOp
eopParserD = do
  _ <- string "->"
  return DArrow

eopParser :: Parser EdgeOp
eopParser = do
  try eopParserU <|> eopParserD

edgeSubgraphParser :: Parser EdgeRHS
edgeSubgraphParser = do
  op <- eopParser
  ESub op <$> getSubgraph

edgeNodeParser :: Parser EdgeRHS
edgeNodeParser = do
  op <- eopParser
  ENode op <$> parseNodeID

parseERHS :: Parser EdgeRHS
parseERHS = do
  try edgeSubgraphParser <|> edgeNodeParser

parseAllERHS :: Parser EdgeList
parseAllERHS = do
  some parseERHS

parseAttListEBase :: Parser BaseEdgeStmt
parseAttListEBase = do
  x <- parseAllERHS
  AttrEStmt x <$> finalAttrListParser

parseBasicEBase :: Parser BaseEdgeStmt
parseBasicEBase = do
  BasicEStmt <$> parseAllERHS

parseEBase :: Parser BaseEdgeStmt
parseEBase = do
  try parseAttListEBase <|> parseBasicEBase

parseEdgeStmtNodeId :: Parser EdgeStmt
parseEdgeStmtNodeId = do
  id <- parseNodeID
  NEStmt id <$> parseEBase

parseEdgeStmtSubgraph :: Parser EdgeStmt
parseEdgeStmtSubgraph = do
  sub <- getSubgraph
  SStmt sub <$> parseEBase

parseEdgeStmt :: Parser Stmt
parseEdgeStmt = do
  x <- try parseEdgeStmtSubgraph <|> parseEdgeStmtNodeId
  return $ EdgeStmt' x

---

-- Case insensitive mappers
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString str = try (mapM caseInsensitiveChar str)

-- Graph mapper
parseBasicGraphTypeNoId :: String -> GraphType -> Parser GraphBase
parseBasicGraphTypeNoId str ty = do
  skipWhitespace
  _ <- caseInsensitiveString str
  skipWhitespace
  list <- statementListParser
  if edgeDirectionEnforcer ty list && subgraphChecker [] list
    && correctnessOfGraph list ty
    then return $ BGraphBase (GraphBase' ty list)
    else fail "failed graph properties"

parseBasicGraphTypeId :: String -> GraphType -> Parser GraphBase
parseBasicGraphTypeId str ty = do
  skipWhitespace
  _ <- caseInsensitiveString str
  skipWhitespace
  id <- parseGraphVizID
  list <- statementListParser
  if edgeDirectionEnforcer ty list && subgraphChecker [id] list
    && correctnessOfGraph list ty
    then return $ BGraphBaseID (GraphBase' ty list) id
    else fail "failed graph properties"

getAllEdges :: StmtList -> EdgeListTup
getAllEdges = foldr stmtGo []
  where
    stmtGo :: Stmt -> EdgeListTup -> EdgeListTup
    stmtGo (EdgeStmt' list) adList = edgeStmt list ++ adList
    stmtGo (Subgraph' subgraph) adList = subgraphStmt subgraph ++ adList
    stmtGo _ adList = adList

    -- subgraph part
    subgraphStmt :: Subgraph -> EdgeListTup
    subgraphStmt (SubgraphID _ subStmtList) = subchecker subStmtList
    subgraphStmt (Subgraph subStmtList) = subchecker subStmtList

    subchecker :: StmtList -> EdgeListTup
    subchecker subStmtList = getAllEdges subStmtList

    -- edgelist part
    edgeStmt :: EdgeStmt -> EdgeListTup
    edgeStmt (NEStmt _ stmt) = edgeList stmt
    edgeStmt (SStmt _ stmt) = edgeList stmt

    edgeList :: BaseEdgeStmt -> EdgeListTup
    edgeList (BasicEStmt list) = listList list
    edgeList (AttrEStmt list _) = listList list

    listList :: EdgeList -> EdgeListTup
    listList elist = zip elist $ tail elist

edgeDirectionEnforcer :: GraphType -> StmtList -> Bool
edgeDirectionEnforcer ty = foldr stmtGo True
  where
    stmtGo :: Stmt -> Bool -> Bool
    stmtGo _ False = False
    stmtGo (EdgeStmt' list) _ = edgeStmt list
    stmtGo (Subgraph' subgraph) _ = subgraphStmt subgraph
    stmtGo _ _ = True

    -- Subgraph part
    subgraphStmt :: Subgraph -> Bool
    subgraphStmt (SubgraphID _ subStmtList) = subchecker subStmtList
    subgraphStmt (Subgraph subStmtList) = subchecker subStmtList

    subchecker :: StmtList -> Bool
    subchecker list = foldr stmtGo True list

    -- Edge part
    edgeStmt :: EdgeStmt -> Bool
    edgeStmt (NEStmt _ stmt) = edgeList stmt
    edgeStmt (SStmt sub stmt) = edgeList stmt && subgraphStmt sub

    edgeList :: BaseEdgeStmt -> Bool
    edgeList (BasicEStmt list) = listScanner list
    edgeList (AttrEStmt list _) = listScanner list

    listScanner :: EdgeList -> Bool
    listScanner elist = foldr listScannerGo True elist
      where
        listScannerGo :: EdgeRHS -> Bool -> Bool
        listScannerGo _ False = False
        listScannerGo (ENode op _) _ = comparer ty op
        listScannerGo (ESub op _) _ = comparer ty op

        comparer :: GraphType -> EdgeOp -> Bool
        comparer Digraph DArrow = True
        comparer Graph UArrow = True
        comparer _ _ = False

subgraphChecker :: [GraphVizID] -> StmtList -> Bool
subgraphChecker listId = foldr stmtGo True
  where
    stmtGo :: Stmt -> Bool -> Bool
    stmtGo _ False = False
    stmtGo (EdgeStmt' list) _ = edgeStmt list
    stmtGo (Subgraph' subgraph) _ = subgraphStmt subgraph
    stmtGo _ _ = True

    -- Subgraph part
    subgraphStmt :: Subgraph -> Bool
    subgraphStmt (SubgraphID graphId subStmtList) =
      notElem graphId listId && subgraphChecker (graphId : listId) subStmtList
    subgraphStmt (Subgraph subStmtList) = subgraphChecker listId subStmtList

    -- Edge part
    edgeStmt :: EdgeStmt -> Bool
    edgeStmt (SStmt sub _) = subgraphStmt sub
    edgeStmt _ = True

graphBaseParser :: Parser GraphBase
graphBaseParser = do
  try (parseBasicGraphTypeId "graph" Graph)
    <|> try (parseBasicGraphTypeId "digraph" Digraph)
    <|> try (parseBasicGraphTypeNoId "graph" Graph)
    <|> parseBasicGraphTypeNoId "digraph" Digraph

correctnessOfGraph :: StmtList -> GraphType -> Bool
correctnessOfGraph x Graph = findIfCorrect (getAllEdges x) findUndirected
correctnessOfGraph x Digraph = findIfCorrect (getAllEdges x) findDirected

findIfCorrect :: EdgeListTup -> ((EdgeRHS, EdgeRHS) -> EdgeListTup -> Bool) -> Bool
findIfCorrect list fn = foldr go True list
  where
    go :: (EdgeRHS, EdgeRHS) -> Bool -> Bool
    go _ False = False
    go tup _ = fn tup list

findUndirected :: (EdgeRHS, EdgeRHS) -> EdgeListTup -> Bool
findUndirected (a, b) list = case find finder list of
  Just _ -> False
  Nothing -> True
  where
    finder :: (EdgeRHS, EdgeRHS) -> Bool
    finder (a', b') = ((a == a') && (b == b')) || ((a == b') && (b == a'))

findDirected :: (EdgeRHS, EdgeRHS) -> EdgeListTup -> Bool
findDirected (a, b) list = case find finder list of
  Just _ -> False
  Nothing -> True
  where
    finder :: (EdgeRHS, EdgeRHS) -> Bool
    finder (a', b') = (a == a') && (b == b')

graphParserStrict :: Parser Graph
graphParserStrict = do
  skipWhitespace
  _ <- caseInsensitiveString "strict"
  skipWhitespace
  StrictGraph <$> graphBaseParser

graphParserNormal :: Parser Graph
graphParserNormal = do
  NonStrictGraph <$> graphBaseParser

graphParser :: Parser Graph
graphParser = do
  try graphParserStrict <|> graphParserNormal
