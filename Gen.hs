{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Lens hiding ((<.>))
import Control.Monad (guard)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..))
import Development.Shake
import Development.Shake.FilePath
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Pretty (pretty)
import Language.Java.Syntax
import Text.PrettyPrint (render)
import Text.Parsec.Error (ParseError)
import Data.Bifoldable (bitraverse_)
import System.Directory (createDirectoryIfMissing, makeAbsolute)

newtype ClassInfo
  = ClassInfo { classTypeParams :: [TypeParam]
              }

type TVar
  = Char

type Fresh a
  = StateT TVar (Reader ClassInfo) a

initialFresh :: TVar
initialFresh =
  'ω'

lambda :: String -> Exp -> Exp
lambda a =
  Lambda (LambdaSingleParam $ Ident a) . LambdaExpression

id' :: Exp
id' =
  lambda "a" $ n' "a"

const' :: Exp -> Exp
const' =
  lambda "a"

n :: String -> Name
n =
  Name . (:[]) . Ident

n' :: String -> Exp
n' =
  ExpName . n

call :: Name -> [Argument] -> Exp
call i =
  MethodInv . MethodCall i

actual :: ClassType -> TypeArgument
actual =
  ActualType . ClassRefType

con :: String -> [TypeArgument] -> ClassType
con t as =
  ClassType [(Ident t, as)]

typeRef :: String -> ClassType
typeRef =
  flip con []

con1 :: String -> [String] -> ClassType -> ClassType
con1 t as a =
  con t $ (actual . typeRef <$> as) ++ [actual a]

con2 :: String -> [String] -> ClassType -> ClassType -> ClassType
con2 t as a b =
  con t $ (actual . typeRef <$> as) ++ [actual a, actual b]

func :: ClassType -> ClassType -> ClassType
func =
  con2 "Function" []

formal :: Type -> String -> FormalParam
formal t =
  FormalParam [Final] t False . VarId . Ident

formalClass :: ClassType -> String -> FormalParam
formalClass =
  formal . RefType . ClassRefType

formalPrim :: PrimType -> String -> FormalParam
formalPrim =
  formal . PrimType

staticMethod :: String -> ClassType -> [FormalParam] -> Exp -> Fresh MemberDecl
staticMethod i t =
  method' i (RefType $ ClassRefType t) True

method :: String -> ClassType -> [FormalParam] -> Exp -> Fresh MemberDecl
method i t =
  method' i (RefType $ ClassRefType t) False

method' :: String -> Type -> Bool -> [FormalParam] -> Exp -> Fresh MemberDecl
method' i t c as b = do
  s <- get
  let ps = flip TypeParam [] . Ident . (:[]) <$> [succ s..initialFresh]
  r <- asks classTypeParams
  let ps' = (if c then r else []) ++ ps
  return . MethodDecl (Public : [Static | c]) ps' (Just t) (Ident i) as [] Nothing . MethodBody . Just . Block . (:[]) . BlockStmt . Return $ Just b

freshTV :: Fresh ClassType
freshTV = do
  s <- get
  modify pred
  return $ typeRef [s]

runFresh :: Fresh c -> Reader ClassInfo c
runFresh =
  flip evalStateT initialFresh

runCon2 :: ((ClassType -> ClassType -> ClassType) -> ClassType -> ClassType -> Fresh c) -> String -> [String] -> String -> String -> ClassInfo -> c
runCon2 f i as a =
  runReader . runFresh . f (con2 i as) (typeRef a) . typeRef

runCon1 :: ((ClassType -> ClassType) -> ClassType -> Fresh c) -> String -> [String] -> String -> ClassInfo -> c
runCon1 f i as =
  runReader . runFresh . f (con1 i as) . typeRef

unit' :: Exp
unit' =
  FieldAccess . ClassFieldAccess (n "Unit") $ Ident "unit"

primaryCall :: String -> String -> [Argument] -> Exp
primaryCall i j =
  MethodInv . PrimaryMethodCall (n' i) [] (Ident j)

-- Derived methods

bifunctorFirst :: (ClassType -> t -> ClassType) -> ClassType -> t -> Fresh MemberDecl
bifunctorFirst f a b = do
  o <- freshTV
  method "first" (f o b) [formalClass (func a o) "ao"] $ call (n "bimap") [n' "ao", id']

bifunctorSecond :: (t -> ClassType -> ClassType) -> t -> ClassType -> Fresh MemberDecl
bifunctorSecond f a b = do
  o <- freshTV
  method "second" (f a o) [formalClass (func b o) "bo"] $ call (n "bimap") [id', n' "bo"]

functorAs :: (ClassType -> ClassType) -> t -> Fresh MemberDecl
functorAs f _ = do
  o <- freshTV
  method "as" (f o) [formalClass o "o"] $ call (n "map") [const' $ n' "o"]

functorVoided :: (ClassType -> ClassType) -> t -> Fresh MemberDecl
functorVoided f _ =
  method "voided" (f $ typeRef "Unit") [] $ call (n "as") [unit']

monadAp :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
monadAp f a = do
  o <- freshTV
  method "ap" (f o) [formalClass (f (func a o)) "f"] $ primaryCall "f" "bind" [lambda "g" $ call (n "bind") [lambda "a" $ call (n "point") [primaryCall "g" "apply" [n' "a"]]]]

bindJoin :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
bindJoin f a =
  staticMethod "join" (f a) [formalClass (f (f a)) "e"] $ primaryCall "e" "bind" [id']

bindForever :: (ClassType -> ClassType) -> t -> Fresh MemberDecl
bindForever f _ = do
  o <- freshTV
  method "forever" (f o) [] $ call (n "bind") [const' $ call (n "forever") []]

applicativeMap :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
applicativeMap f a = do
  o <- freshTV
  method "map" (f o) [formalClass (func a o) "f"] $ call (n "ap") [call (n "point") [n' "f"]]

applicativeWhen :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
applicativeWhen f _ =
  method "when" (f $ typeRef "Unit") [formalPrim BooleanT "b", formalClass (f $ typeRef "Unit") "e"] $ Cond (n' "b") (n' "e") (call (n "point") [unit'])

applicativeUnless :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
applicativeUnless f _ =
  method "unless" (f $ typeRef "Unit") [formalPrim BooleanT "b", formalClass (f $ typeRef "Unit") "e"] $ call (n "when") [PreNot (n' "b"), n' "e"]

applyBefore :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
applyBefore f a = do
  o <- freshTV
  method "before" (f a) [formalClass (f o) "b"] $ call (n "ap") [primaryCall "b" "map" [lambda "c" . lambda "a" $ n' "a"]]

applyThen :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
applyThen f _ = do
  o <- freshTV
  method "then" (f o) [formalClass (f o) "b"] $ call (n "ap") [primaryCall "b" "map" [lambda "a" . lambda "c" $ n' "a"]]

applyMap2 :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
applyMap2 f a = do
  o <- freshTV
  p <- freshTV
  method "map2" (f p) [formalClass (func a (func o p)) "f", formalClass (f o) "c"] $ primaryCall "c" "ap" [call (n "map") [n' "f"]]

applyMap3 :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
applyMap3 f a = do
  o <- freshTV
  p <- freshTV
  q <- freshTV
  method "map3" (f q) [formalClass (func a (func o (func p q))) "f", formalClass (f o) "c", formalClass (f p) "d"] $ primaryCall "d" "ap" [primaryCall "c" "ap" [call (n "map") [n' "f"]]]

applyMap4 :: (ClassType -> ClassType) -> ClassType -> Fresh MemberDecl
applyMap4 f a = do
  o <- freshTV
  p <- freshTV
  q <- freshTV
  r <- freshTV
  method "map4" (f r) [formalClass (func a (func o (func p (func q r)))) "f", formalClass (f o) "c", formalClass (f p) "d", formalClass (f q) "e"] $ primaryCall "e" "ap" [primaryCall "d" "ap" [primaryCall "c" "ap" [call (n "map") [n' "f"]]]]

foldable1FoldMap :: t -> ClassType -> Fresh MemberDecl
foldable1FoldMap _ a = do
  r <- freshTV
  method "foldMap" r [formalClass (con1 "Monoid" [] r) "m", formalClass (func a r) "f"] $ call (n "foldMap1") [n' "m", n' "f"]

foldable1Folded :: t -> ClassType -> Fresh MemberDecl
foldable1Folded _ a =
  method "folded1" a [formalClass (con1 "Semigroup" [] a) "m"] $ call (n "foldMap1") [n' "m", lambda "a" $ n' "a"]

foldableFolded :: t -> ClassType -> Fresh MemberDecl
foldableFolded _ a =
  method "folded" a [formalClass (con1 "Monoid" [] a) "m"] $ call (n "foldMap") [n' "m", lambda "a" $ n' "a"]

foldableMonoidFunction :: String -> t -> ClassType -> Fresh MemberDecl
foldableMonoidFunction name _ a =
  method' name (PrimType BooleanT) False [formalClass (func a (typeRef "Boolean")) "f"] $ call (n "foldMap") [FieldAccess . ClassFieldAccess (n "Monoid") $ Ident name, n' "f"]

foldableAny :: t -> ClassType -> Fresh MemberDecl
foldableAny =
  foldableMonoidFunction "any"

foldableAll :: t -> ClassType -> Fresh MemberDecl
foldableAll =
  foldableMonoidFunction "all"

foldableIsEmpty :: t -> u -> Fresh MemberDecl
foldableIsEmpty _ _ =
  method' "isEmpty" (PrimType BooleanT) False [] $ call (n "any") [const' . Lit $ Boolean True]

foldableLength :: t -> u -> Fresh MemberDecl
foldableLength _ _ =
  method' "length" (PrimType IntT) False [] $ call (n "foldMap") [FieldAccess . ClassFieldAccess (n "Monoid") $ Ident "sumInteger", const' . Lit $ Int 1]

-- Lenses

typeDecls :: Lens' CompilationUnit [TypeDecl]
typeDecls =
  lens (\(CompilationUnit _ _ t) -> t) (\(CompilationUnit a b _) t -> CompilationUnit a b t)

classTypeDecl :: Prism' TypeDecl ClassDecl
classTypeDecl =
  prism' ClassTypeDecl f
  where
    f (ClassTypeDecl t) = Just t
    f _ = Nothing

classDeclBody :: Traversal' ClassDecl ClassBody
classDeclBody f (ClassDecl a b c d e x) =
  ClassDecl a b c d e <$> f x
classDeclBody _ o =
  pure o

classDeclIdent :: Traversal' ClassDecl Ident
classDeclIdent f (ClassDecl a x b c d e) =
  (\x' -> ClassDecl a x' b c d e) <$> f x
classDeclIdent _ o =
  pure o

classDeclTypeParams :: Traversal' ClassDecl [TypeParam]
classDeclTypeParams f (ClassDecl a b x c d e) =
  (\x' -> ClassDecl a b x' c d e) <$> f x
classDeclTypeParams _ o =
  pure o

classBodyDecls :: Iso' ClassBody [Decl]
classBodyDecls =
  iso (\(ClassBody d) -> d) ClassBody

identString :: Iso' Ident String
identString =
  iso (\(Ident s) -> s) Ident

typeParamIdent :: Lens' TypeParam Ident
typeParamIdent =
  lens (\(TypeParam i _) -> i) (\(TypeParam _ ts) i -> TypeParam i ts)

decl :: Traversal' CompilationUnit ClassDecl
decl =
  typeDecls . _head . classTypeDecl

memberDecl' :: Prism' Decl MemberDecl
memberDecl' =
  prism' MemberDecl f
  where
    f (MemberDecl x) = Just x
    f _ = Nothing

memberDecls :: Traversal' CompilationUnit [Decl]
memberDecls =
  decl . classDeclBody . classBodyDecls

methodName :: Traversal' MemberDecl Ident
methodName h (MethodDecl a b c x d e f g) =
  (\x' -> MethodDecl a b c x' d e f g) <$> h x
methodName _ o =
  pure o

xi :: (Applicative f, Reversing a, Ixed a) => Index a -> (IxValue a -> f (IxValue a)) -> a -> f a
xi i =
  reversed . ix i

droppingRight :: (Applicative f, Foldable f1, Reversing a, Reversing (f1 a), Indexable Int p, Contravariant f) => Int -> p a (f a) -> f1 a -> f (f1 a)
droppingRight i =
  reversed . dropping i folded . reversed

-- Main

transformAST :: CompilationUnit -> CompilationUnit
transformAST ast =
  appEndo
    (foldMap
      (Endo . derive)
      [ functor
      , apply
      , applicative
      , bind
      , monad
      , bifunctor
      , foldable
      , foldable1
      ])
    ast
  where
    derive f ast' =
      ast' & memberDecls <>~ fromMaybe [] (f ast')
    params =
      ast ^.. decl . classDeclTypeParams . folded
    paramStrings =
      params ^.. folded . typeParamIdent . identString
    hasMethod s =
      has $ memberDecls . folded . memberDecl' . methodName . identString . only s
    typeCon1 f = do
      i <- ast ^? decl . classDeclIdent . identString
      a <- paramStrings ^? xi 0
      let ts = paramStrings ^.. droppingRight 1
      xs <- f
      pure $ (\l -> MemberDecl . runCon1 l i ts a $ ClassInfo params) <$> xs
    typeCon2 f = do
      i <- ast ^? decl . classDeclIdent . identString
      a <- paramStrings ^? xi 1
      b <- paramStrings ^? xi 0
      let ts = paramStrings ^.. droppingRight 2
      xs <- f
      pure $ (\l -> MemberDecl . runCon2 l i ts a b $ ClassInfo params) <$> xs
    bifunctor ast' = typeCon2 $ do
      guard (hasMethod "bimap" ast')
      pure
        [ bifunctorFirst
        , bifunctorSecond
        ]
    monad ast' = typeCon1 $ do
      guard (hasMethod "point" ast')
      guard (hasMethod "bind" ast')
      pure
        [ monadAp
        ]
    bind ast' = typeCon1 $ do
      guard (hasMethod "bind" ast')
      pure
        [ bindJoin
        , bindForever
        ]
    applicative ast' = typeCon1 $ do
      guard (hasMethod "ap" ast')
      guard (hasMethod "point" ast')
      pure
        [ applicativeMap
        , applicativeWhen
        , applicativeUnless
        ]
    apply ast' = typeCon1 $ do
      guard (hasMethod "ap" ast')
      pure
        [ applyBefore
        , applyThen
        , applyMap2
        , applyMap3
        , applyMap4
        ]
    functor ast' = typeCon1 $ do
      guard (hasMethod "map" ast')
      pure
        [ functorAs
        , functorVoided
        ]
    foldable1 ast' = typeCon1 $ do
      guard (hasMethod "foldMap1" ast')
      pure
        [ foldable1FoldMap
        , foldable1Folded
        ]
    foldable ast' = typeCon1 $ do
      guard (hasMethod "foldMap" ast')
      pure
        [ foldableFolded
        , foldableAny
        , foldableAll
        , foldableIsEmpty
        , foldableLength
        ]

run :: String -> Either ParseError String
run c =
  render . pretty . transformAST <$> parser compilationUnit c

srcJava :: FilePath
srcJava =
  "src" </> "main" </> "java"

classes :: FilePath
classes =
  "build" </> "classes"

main :: IO ()
main = shake (shakeOptions { shakeChange = ChangeDigest }) $ do
  version <- liftIO (filter (not . isSpace) <$> readFile "version.txt")

  let
    jar t =
      "lam-" <> t <> version <.> "jar"
    sha1 =
      (<.> "sha1")
    mainJar =
      jar ""
    javadocJar =
      jar "javadoc-"
    sourcesJar =
      jar "sources-"

  want
    [ "build" </> sha1 mainJar
    , "build" </> sha1 javadocJar
    , "build" </> sha1 sourcesJar
    ]

  phony "clean" $ removeFilesAfter "build" ["//*"]

  "build" </> "*.sha1" %> \out -> do
    let c = fromMaybe out (stripExtension "sha1" out)
    need [c]
    sha1sum <- cmd "sha1sum" c
    traverse_ (liftIO . writeFile' out) (take 1 (words (fromStdout sha1sum)))

  "build" </> sourcesJar %> \out -> do
    cs <- getDirectoryFiles "" [srcJava <//> "*.java"]
    need (("build" </>) <$> cs)
    out' <- liftIO (makeAbsolute out)
    cmd_ (Cwd ("build" </> srcJava)) "jar cf" out' (makeRelative srcJava <$> cs)

  "build" </> "doc" </> "index.html" %> \_ -> do
    cs <- getDirectoryFiles "" ["build" </> srcJava </> "//*.java"]
    need cs
    liftIO (removeFiles ("build" </> "doc") ["//*"])
    liftIO (createDirectoryIfMissing False ("build" </> "doc"))
    out' <- liftIO (makeAbsolute ("build" </> "doc"))
    cmd_ (Cwd ("build" </> srcJava)) "javadoc -notimestamp -charset utf-8 -subpackages . -d" out' "lam"

  "build" </> javadocJar %> \out -> do
    need ["build" </> "doc" </> "index.html"]
    out' <- liftIO (makeAbsolute out)
    cmd_ (Cwd ("build" </> "doc")) "jar cf " [out'] "."

  "build" </> mainJar %> \out -> do
    cs <- getDirectoryFiles srcJava ["//*.java"]
    need ((\c -> classes </> c -<.> "class") <$> cs)
    out' <- liftIO (makeAbsolute out)
    cs' <- getDirectoryFiles classes ["//*.class"]
    cmd_ (Cwd classes) "jar cf" [out'] cs'

  batch 10 (classes <//> "*.class" %>) pure $ \outs -> do
    let
      srcs =
        (\o -> makeRelative classes o -<.> "java") <$> outs
    cs <- getDirectoryFiles "" [srcJava <//> "*.java"]
    need (("build" </>) <$> cs)
    liftIO (createDirectoryIfMissing False classes)
    dest <- liftIO (makeAbsolute classes)
    cmd_ (Cwd ("build" </> srcJava)) "javac -source 8 -target 8 -d" (dest : srcs)

  "build" </> srcJava <//> "*.java" %> \out -> do
    alwaysRerun
    let src = makeRelative "build" out
    need [src]
    content <- readFile' src
    let content' = run content
    bitraverse_ (fail . show) (writeFile' out) content'
