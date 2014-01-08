
{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts
           , CPP
           #-}

module Entologic.Convert where

import qualified Data.Text as T
import Data.Text (Text(..))

import qualified Language.Java.Syntax as J

import Entologic.Ast

class Convertable a b where
    convert :: a -> b

instance (Functor f, Convertable a b) => Convertable (f a) (f b) where
    convert = fmap convert

instance (Convertable a b, Convertable c d) => Convertable (a, c) (b, d) where
    convert (a, b) = (convert a, convert b)

instance Convertable J.CompilationUnit Program where
    convert (J.CompilationUnit pkg imps typds) =
        CompilationUnit (convert pkg) (convert imps) (convert typds)

instance Convertable a b => Convertable a (AN b) where
    convert a = (Node $ convert a, Area Nothing Nothing)

instance Convertable J.PackageDecl [Text'] where
    convert (J.PackageDecl n) = convert n

instance Convertable J.Ident a => Convertable J.Name [a] where
    convert (J.Name idents) = convert idents

instance Convertable J.Ident Text where
    convert (J.Ident s) = T.pack s

instance Convertable String Text where
    convert = T.pack

instance Convertable J.ImportDecl Import where
    convert (J.ImportDecl True name True) = ImportStaticAll $ convert name
    convert (J.ImportDecl False name True) = ImportAll $ convert name
    convert (J.ImportDecl True name False) = ImportStatic $ convert name
    convert (J.ImportDecl False name False) = Import $ convert name

instance Convertable J.TypeDecl TypeDeclaration where
    convert (J.ClassTypeDecl c) = convert c
--    convert (J.InterfaceTypeDecl i) = convert i

($>) :: Convertable a b => (b -> c) -> a -> c
func $> val = func $ convert val

instance Convertable J.ClassDecl TypeDeclaration where
    convert (J.ClassDecl mods name gParams sClass interfs body) =
        TDCls $ Class $> mods $> name $> gParams $> sClass $> interfs $> body

instance Convertable J.ClassBody [Member] where
    convert (J.ClassBody decls) = convert decls

instance Convertable J.Decl InClassDecl where
    convert (J.MemberDecl m) = MemberDecl $> m
    convert (J.InitDecl True b) = StaticInitBlock $> b
    convert (J.InitDecl False b) = InitBlock $> b

instance Convertable J.MemberDecl where
    convert (J.FieldDecl mods typ vd) = Field
    convert (J.MethodDecl mods genParams typ name params exceptions body) =
        Function $> mods $> typ $> name $> params $> body

instance Convertable J.FormalParam ParamDecl where
    convert (FormalParam mods typ va name) =
        ParamDecl $> name $> typ $> mods $> Nothing $> varargs va
      where varargs True = Just Varargs
            varargs False = Nothing

#define CV(thing) convert J.thing = thing
instance Convertable J.Modifier Modifier where
    CV(Public)
    CV(Private)
    CV(Protected)
    CV(Abstract)
    CV(Final)
    CV(Static)
    CV(StrictFP)
    CV(Transient)
    CV(Volatile)
    CV(Native)
    convert J.Synchronised = Synchronized

instance Convertable J.TypeParam GenericParamDecl where
    convert = const GenericParamDecl

instance Convertable J.TypeArgument GenericParam where
    convert = const GenericParam

instance Convertable J.Type Type where
    convert (J.RefType rt) = convert rt
    convert (J.PrimType t) = PrimType (convert t)

instance Convertable J.RefType Type where
    convert (J.ClassRefType (J.ClassType parts)) = ClassType $> parts
    convert (J.ArrayType typ) = ArrayType $> typ

instance Convertable J.PrimType PrimType where
    CV(IntT)
    CV(LongT)
    CV(ShortT)
    CV(DoubleT)
    CV(ByteT)
    CV(FloatT)
    CV(CharT)
    CV(BooleanT)

