module Ivory.Compile.C.Gen.Const (makeTargetConst, makeTargetConstIf) where

import           Data.Loc (noLoc)
import qualified Language.C.Syntax as C

makeTargetConstIf :: Bool -> C.Type -> C.Type
makeTargetConstIf c t = if c then makeTargetConst t else t

makeTargetConst :: C.Type -> C.Type
makeTargetConst = modifyTargetQuals (C.Tconst noLoc :)

modifyTargetQuals :: ([C.TypeQual] -> [C.TypeQual]) -> C.Type -> C.Type
modifyTargetQuals f typ@(C.Type _ decl _) = case decl of
  C.Array _ _ decl1 _ -> go decl1
  C.Ptr _ decl1 _     -> go decl1
  _                   -> typ
  where
    go decl1 = case decl1 of
      C.DeclRoot{} ->
        -- `typ` is rank 1 pointer, modify base
        (modifyTypeDeclSpec . modifyDeclSpecQuals) f typ
      C.Ptr{} ->
        -- `typ` is rank >1 pointer, modify second pointer level
        (modifyTypeDecl . modifyNestedDecl . modifyPtrQuals) f typ
      _ ->
        -- nothing to do here
        typ
modifyTargetQuals _ typ@C.AntiType{} = internalError typ

modifyDeclSpecQuals
  :: ([C.TypeQual] -> [C.TypeQual]) -> C.DeclSpec -> C.DeclSpec
modifyDeclSpecQuals f declSpec = case declSpec of
  C.DeclSpec storage quals spec loc -> C.DeclSpec storage (f quals) spec loc
  C.AntiDeclSpec{}                  -> internalError declSpec
  C.AntiTypeDeclSpec{}              -> internalError declSpec

modifyNestedDecl :: (C.Decl -> C.Decl) -> C.Decl -> C.Decl
modifyNestedDecl f decl0 = case decl0 of
  C.Array quals size decl loc -> C.Array quals size (f decl) loc
  C.Ptr quals decl loc        -> C.Ptr quals (f decl) loc
  _                           -> decl0

modifyPtrQuals :: ([C.TypeQual] -> [C.TypeQual]) -> C.Decl -> C.Decl
modifyPtrQuals f (C.Ptr quals decl loc)  = C.Ptr (f quals) decl loc
modifyPtrQuals _ decl                    = decl

modifyTypeDecl :: (C.Decl -> C.Decl) -> C.Type -> C.Type
modifyTypeDecl f (C.Type declSpec decl loc) = C.Type declSpec (f decl) loc
modifyTypeDecl _ typ@C.AntiType{}           = internalError typ

modifyTypeDeclSpec :: (C.DeclSpec -> C.DeclSpec) -> C.Type -> C.Type
modifyTypeDeclSpec f (C.Type declSpec decl loc) = C.Type (f declSpec) decl loc
modifyTypeDeclSpec _ typ@C.AntiType{}           = internalError typ

internalError :: Show a => a -> b
internalError a = error $ "internal language-c-quote data leaked: " ++ show a
