
{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , OverlappingInstances
           , FlexibleContexts
           , CPP
           #-}

module Entologic.Convert where

import qualified Data.Text as T
import Data.Text (Text(..))

import qualified Language.Java.Syntax as J

import Entologic.Ast

#define CV(thing) convert J.thing = thing

class Convertable a b where
    convert :: a -> b

instance Convertable a a where
    convert = id

instance (Functor f, Convertable a b) => Convertable (f a) (f b) where
    convert = fmap convert

instance (Convertable a b, Convertable c d) => Convertable (a, c) (b, d) where
    convert (a, b) = (convert a, convert b)

--instance Convertable a b => Convertable a (Maybe b) where
--    convert = Just . convert

instance Convertable J.CompilationUnit Program where
    convert (J.CompilationUnit pkg imps typds) =
        CompilationUnit (convert pkg) (convert imps) (convert typds)

instance Convertable a b => Convertable a (AN b) where
    convert a = (Node $ convert a, Area Nothing Nothing)

instance Convertable J.PackageDecl [Text'] where
    convert (J.PackageDecl n) = convert n


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

instance Convertable J.ClassBody [InClassDecl'] where
    convert (J.ClassBody decls) = convert decls

instance Convertable J.MethodBody (Maybe Block)

instance Convertable J.Decl InClassDecl where
    convert (J.MemberDecl m) = MemberDecl $> m
    convert (J.InitDecl True b) = StaticInitBlock $> b
    convert (J.InitDecl False b) = InitBlock $> b

instance Convertable J.MemberDecl Member where
    convert (J.FieldDecl mods typ vd) = MField $> Field
    convert (J.MethodDecl mods genParams typ name params exceptions body) =
        (MFunc $>) $ Function $> mods $> typ $> name $> params $> body

instance Convertable J.Block Block where
    convert (J.Block stms) = convert stms

instance Convertable J.BlockStmt Statement where
    convert (J.BlockStmt stmt) = convert stmt

instance Convertable J.Stmt Statement where
    convert (J.ExpStmt exp) = StmExpr $> exp

instance Convertable J.Exp Expression where
    convert (J.BinOp l op r) = BinOp $> op $> l $> r

instance Convertable J.Op InfixOp where
    convert J.Add = Plus
    convert J.Sub = Minus
    CV(Mult)
    CV(Div)
    convert J.Rem = Mod

instance Convertable J.FormalParam ParamDecl where
    convert (J.FormalParam mods typ va name) =
        ParamDecl $> name $> Just (convert typ) $> mods $> Nothing $> varargs va
      where varargs True = Just VarArgs
            varargs False = Nothing

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

instance Convertable J.Ident a => Convertable J.Name [a] where
    convert (J.Name idents) = convert idents

instance Convertable J.Ident Text where
    convert (J.Ident s) = T.pack s

instance Convertable String Text where
    convert = T.pack

instance Convertable J.VarDeclId Text where
    convert (J.VarId id) = convert id
