{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Language.Thrift.Pretty
-- Copyright   :  (c) Abhinav Gupta 2016
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module provides a pretty printer for Thrift IDLs. Most of the printers
-- defined in this module produce output highlighted using ANSI escape codes.
-- Get plain output by using 'Text.PrettyPrint.ANSI.Leijen.plain'.
--
-- Use 'prettyPrintHighlighted' to produce output highlighted using ANSI escape
-- codes. Note that this output will be unparseable and is suitable for printing
-- inside a compatible terminal only. Use 'prettyPrint' if you don't want
-- highlighted output.
--
-- The behavior of the printer can be customized using 'Config' objects.
--
-- The module also exports instances of the 'Pretty' typeclass for elements of
-- the AST.
module Language.Thrift.Pretty
    (
      prettyPrintHighlighted
    , prettyPrint

    -- * Components

    , program

    , header
    , include
    , namespace

    , definition
    , constant
    , typeDefinition
    , service

    , typedef
    , enum
    , struct
    , union
    , exception
    , senum

    , typeReference
    , constantValue

    , docstring

    -- * Configuration

    , Config(..)
    , defaultConfig
    ) where

#if __GLASGOW_HASKELL__ >= 709
import Prelude hiding ((<$>))
#endif

import           Prelude ((<$>))
import           Data.Text (Text)
import qualified Data.Text as Text

import Prettyprinter.Render.Terminal
    ( AnsiStyle
    , bold
    , color
    , colorDull
    , Color(..)
    )
import Prettyprinter
    ( Doc
    , Pretty (..)
    , align
    , dquotes
    , enclose
    , group
    , hcat
    , hsep
    , line
    , nest
    , space
    , vsep
    , (<+>)
    , (<>)
    )

import qualified Language.Thrift.Internal.AST as T

-- | Configuration for the pretty printer.
data Config = Config
    { indentWidth :: Int
    -- ^ Number of spaces to use for indentation.
    } deriving (Show, Ord, Eq)

(<$$>) = \x y -> vsep [x, y]

-- | Default pretty printing configuration.
defaultConfig :: Config
defaultConfig = Config 4

-- | Top-level pretty printer for Thrift documents that uses the default
-- configuration ('defaultConfig') for pretty printing.
prettyPrint :: T.Program ann -> Doc AnsiStyle
prettyPrint = plain . prettyPrintHighlighted

-- | Top-level pretty printer for Thrift documents.
prettyPrintHighlighted :: T.Program ann -> Doc AnsiStyle
prettyPrintHighlighted = program defaultConfig

-- | Pretty print a Thrift IDL.
program :: Config -> T.Program ann -> Doc AnsiStyle
program c T.Program{..} =
    ( if null programHeaders
        then mempty
        else vsep (map header programHeaders) <$> line
    ) <> map (definition c) programDefinitions `sepBy` (line <> line)

instance Pretty (T.Program a) where
    pretty = program defaultConfig

-- | Print the headers for a program.
header :: T.Header ann -> Doc AnsiStyle
header (T.HeaderInclude inc)  = include inc
header (T.HeaderNamespace ns) = namespace ns

instance Pretty (T.Header a) where
    pretty = header

include :: T.Include ann -> Doc AnsiStyle
include T.Include{..} = reserved "include" <+> literal includePath

instance Pretty (T.Include a) where
    pretty = include

namespace :: T.Namespace ann -> Doc AnsiStyle
namespace T.Namespace{..} = hsep
    [reserved "namespace", pretty namespaceLanguage, pretty namespaceName]

instance Pretty (T.Namespace a) where
    pretty = namespace

-- | Print a constant, type, or service definition.
definition :: Config -> T.Definition ann -> Doc AnsiStyle
definition c (T.ConstDefinition cd)  = constant c cd
definition c (T.TypeDefinition def)  = typeDefinition c def
definition c (T.ServiceDefinition s) = service c s

instance Pretty (T.Definition a) where
    pretty = definition defaultConfig

constant :: Config -> T.Const ann -> Doc AnsiStyle
constant c T.Const{..} = constDocstring $$ hsep
    [ reserved "const"
    , typeReference c constValueType
    , declare constName
    , equals
    , constantValue c constValue
    ]

instance Pretty (T.Const a) where
    pretty = constant defaultConfig

service :: Config -> T.Service ann -> Doc AnsiStyle
service c@Config{indentWidth} T.Service{..} =
  serviceDocstring $$
    reserved "service" <+> declare serviceName <> extends <+>
    block indentWidth (line <> line) (map (function c) serviceFunctions) <>
    typeAnnots c serviceAnnotations
  where
    extends = case serviceExtends of
      Nothing   -> mempty
      Just name -> space <> reserved "extends" <+> text name

instance Pretty (T.Service a) where
    pretty = service defaultConfig

-- | Pretty print a function definition.
--
function :: Config -> T.Function ann -> Doc AnsiStyle
function c@Config{indentWidth} T.Function{..} = functionDocstring $$
  oneway <> returnType <+> text functionName <>
    encloseSep
        indentWidth lparen rparen comma
        (map (field c) functionParameters) <>
    exceptions <> typeAnnots c functionAnnotations <> semi
  where
    exceptions = case functionExceptions of
      Nothing -> mempty
      Just es -> space <> reserved "throws" <+>
        encloseSep indentWidth lparen rparen comma (map (field c) es)
    returnType = case functionReturnType of
      Nothing -> reserved "void"
      Just rt -> typeReference c rt
    oneway =
      if functionOneWay
          then reserved "oneway" <> space
          else mempty

instance Pretty (T.Function a) where
    pretty = function defaultConfig

typeDefinition :: Config -> T.Type ann -> Doc AnsiStyle
typeDefinition c td = case td of
  T.TypedefType   t -> c `typedef`   t
  T.EnumType      t -> c `enum`      t
  T.StructType    t -> c `struct`    t
  T.SenumType     t -> c `senum`     t

instance Pretty (T.Type a) where
    pretty = typeDefinition defaultConfig

typedef :: Config -> T.Typedef ann -> Doc AnsiStyle
typedef c T.Typedef{..} = typedefDocstring $$
    reserved "typedef" <+> typeReference c typedefTargetType <+>
    declare typedefName <> typeAnnots c typedefAnnotations

instance Pretty (T.Typedef a) where
    pretty = typedef defaultConfig

enum :: Config -> T.Enum ann -> Doc AnsiStyle
enum c@Config{indentWidth} T.Enum{..} = enumDocstring $$
    reserved "enum" <+> declare enumName <+>
      block indentWidth (comma <> line) (map (enumValue c) enumValues)
    <> typeAnnots c enumAnnotations

instance Pretty (T.Enum a) where
    pretty = enum defaultConfig

struct :: Config -> T.Struct ann -> Doc AnsiStyle
struct c@Config{indentWidth} T.Struct{..} = structDocstring $$
    kind <+> declare structName <+>
      block indentWidth line (map (\f -> field c f <> semi) structFields)
    <> typeAnnots c structAnnotations
  where
    kind = case structKind of
        T.StructKind    -> reserved "struct"
        T.UnionKind     -> reserved "union"
        T.ExceptionKind -> reserved "exception"

instance Pretty (T.Struct a) where
    pretty = struct defaultConfig

union :: Config -> T.Struct ann -> Doc AnsiStyle
union = struct
{-# DEPRECATED union "Use struct." #-}

exception :: Config -> T.Struct ann -> Doc AnsiStyle
exception = struct
{-# DEPRECATED exception "Use struct." #-}

senum :: Config -> T.Senum ann -> Doc AnsiStyle
senum c@Config{indentWidth} T.Senum{..} = senumDocstring $$
    reserved "senum" <+> declare senumName <+>
      encloseSep indentWidth lbrace rbrace comma (map literal senumValues)
    <> typeAnnots c senumAnnotations

instance Pretty (T.Senum a) where
    pretty = senum defaultConfig

field :: Config -> T.Field ann -> Doc AnsiStyle
field c T.Field{..} = fieldDocstring $$ hcat
    [ case fieldIdentifier of
        Nothing -> mempty
        Just i  -> yellow (pretty i) <> colon <> space
    , case fieldRequiredness of
        Nothing -> mempty
        Just r  -> requiredness r <> space
    , typeReference c fieldValueType
    , space
    , text fieldName
    , case fieldDefaultValue of
        Nothing -> mempty
        Just v  -> space <> equals <+> constantValue c v
    , typeAnnots c fieldAnnotations
    ]

instance Pretty (T.Field a) where
    pretty = field defaultConfig

requiredness :: T.FieldRequiredness -> Doc AnsiStyle
requiredness T.Optional = reserved "optional"
requiredness T.Required = reserved "required"

instance Pretty T.FieldRequiredness where
    pretty = requiredness

enumValue :: Config -> T.EnumDef ann -> Doc AnsiStyle
enumValue c T.EnumDef{..} = enumDefDocstring $$
    text enumDefName <> value <> typeAnnots c enumDefAnnotations
  where
    value = case enumDefValue of
      Nothing -> mempty
      Just v  -> space <> equals <+> pretty v

instance Pretty (T.EnumDef a) where
    pretty = enumValue defaultConfig

-- | Pretty print a field type.
typeReference :: Config -> T.TypeReference ann -> Doc AnsiStyle
typeReference c ft = case ft of
  T.DefinedType t _ -> text t

  T.StringType anns _ -> reserved "string" <> typeAnnots c anns
  T.BinaryType anns _ -> reserved "binary" <> typeAnnots c anns
  T.SListType  anns _ -> reserved "slist"  <> typeAnnots c anns
  T.BoolType   anns _ -> reserved "bool"   <> typeAnnots c anns
  T.ByteType   anns _ -> reserved "byte"   <> typeAnnots c anns
  T.I16Type    anns _ -> reserved "i16"    <> typeAnnots c anns
  T.I32Type    anns _ -> reserved "i32"    <> typeAnnots c anns
  T.I64Type    anns _ -> reserved "i64"    <> typeAnnots c anns
  T.DoubleType anns _ -> reserved "double" <> typeAnnots c anns

  T.MapType k v anns _ ->
    reserved "map"
        <> enclose langle rangle
            (typeReference c k <> comma <+> typeReference c v)
        <> typeAnnots c anns
  T.SetType v anns _ ->
    reserved "set"
        <> enclose langle rangle (typeReference c v)
        <> typeAnnots c anns
  T.ListType v anns _ ->
    reserved "list"
        <> enclose langle rangle (typeReference c v)
        <> typeAnnots c anns

instance Pretty (T.TypeReference a) where
    pretty = typeReference defaultConfig

-- | Pretty print a constant value.
constantValue :: Config -> T.ConstValue ann -> Doc AnsiStyle
constantValue c@Config{indentWidth} value = case value of
  T.ConstInt        i _ -> pretty i
  T.ConstFloat      f _ -> pretty  f
  T.ConstLiteral    l _ -> literal l
  T.ConstIdentifier i _ -> text    i
  T.ConstList      vs _ ->
    encloseSep indentWidth lbracket rbracket comma $ map (constantValue c) vs
  T.ConstMap       vs _ ->
    encloseSep indentWidth lbrace rbrace comma $
      map (\(k, v) -> constantValue c k <> colon <+> constantValue c v) vs

instance Pretty (T.ConstValue a) where
    pretty = constantValue defaultConfig

typeAnnots :: Config -> [T.TypeAnnotation] -> Doc AnsiStyle
typeAnnots _ [] = mempty
typeAnnots Config{indentWidth} anns =
    space <> encloseSep indentWidth lparen rparen comma (map typeAnnot anns)

typeAnnot :: T.TypeAnnotation -> Doc AnsiStyle
typeAnnot T.TypeAnnotation{..} =
    text typeAnnotationName <> value
  where
    value = case typeAnnotationValue of
        Nothing -> mempty
        Just v  -> space <> equals <+> literal v

instance Pretty T.TypeAnnotation where
    pretty = typeAnnot

literal :: Text -> Doc AnsiStyle
literal = color Cyan . dquotes . text
    -- TODO: escaping?

text :: Text -> Doc AnsiStyle
text = pretty . Text.unpack

reserved :: String -> Doc AnsiStyle
reserved = color Magenta . pretty

op :: String -> Doc AnsiStyle
op = color Yellow . pretty

declare :: Text -> Doc AnsiStyle
declare = bold . text

($$) :: T.Docstring -> Doc AnsiStyle -> Doc AnsiStyle
($$) Nothing y = y
($$) (Just t) y =
    if Text.null t'
        then y
        else docstring t' <$> y
  where
    t' = Text.strip t

infixr 1 $$

docstring :: Text -> Doc AnsiStyle
docstring = colorDull Blue . wrapComments . Text.lines
  where
    wrapComments ls = align . vsep
      $ text "/**"
      : map (\l -> text " *" <+> text l) ls
     ++ [text " */"]

block :: Int -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
block indent s items = vsep
    [ lbrace
    , nest indent (items `sepBy` s)
    , rbrace
    ]

sepBy :: [Doc AnsiStyle] -> Doc AnsiStyle -> Doc AnsiStyle
sepBy [] _     = mempty
sepBy [x] _    = x
sepBy (x:xs) s = x <> s <> sepBy xs s

encloseSep :: Int -> Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
encloseSep _ left right _ [] = left <> right
encloseSep _ left right _ [v] = left <> v <> right
encloseSep indent left right s vs = group $
    nest indent (fmap left <$> go vs) <$$> right
  where 
        go :: [Doc AnsiStyle] -> Doc AnsiStyle
        go []     = mempty
        go [x]    = x
        go (x:xs) = (x <> s) <$> go xs

lbrace :: Doc AnsiStyle
lbrace = op "{"

rbrace :: Doc AnsiStyle
rbrace = op "}"

lparen :: Doc AnsiStyle
lparen = op "("

rparen :: Doc AnsiStyle
rparen = op ")"

lbracket :: Doc AnsiStyle
lbracket = op "["

rbracket :: Doc AnsiStyle
rbracket = op "]"

langle :: Doc AnsiStyle
langle = op "<"

rangle :: Doc AnsiStyle
rangle = op ">"

comma :: Doc AnsiStyle
comma = op ","

semi :: Doc AnsiStyle
semi = op ";"

colon :: Doc AnsiStyle
colon = op ":"

equals :: Doc AnsiStyle
equals = op "="
