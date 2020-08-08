{-# LANGUAGE TemplateHaskell, CPP #-}
{- |
Module          : Foreign.C.Structs.Templates
Description     : Create C structs from Haskell
Copyright       : (c) Simon Plakolb, 2020
License         : MIT
Maintainer      : s.plakolb@gmail.com
Stability       : beta

This module exposes the template haskell framework to create Struct types.
-}
module Foreign.C.Structs.Templates
    (structT, acs)
where

import Language.Haskell.TH

import Foreign.Storable (Storable, peek, poke, sizeOf, alignment)
import Foreign.Ptr (castPtr)
import Foreign.C.Structs.Utils (next, sizeof, fmax)

-- | All @StructN@ types and their instances of 'Storable' are declared using 'structT'.
-- It can theoretically create C structs with an infinite number of fields.
-- The parameter of 'structT' is the number of fields the struct type should have.
-- Its constructor and type will both be named @StructN@ where N is equal to the argument to 'structT'.
structT :: Int -> DecsQ
structT = return . zipWith ($) [structTypeT, storableInstanceT] . repeat

-- | Access function for fields of a @StructN@ where @N@ is the number of fields in the struct.
-- N is the first argument passed to 'acs', while the second is the field number.
-- The first field has number 1, the second 2 and so on.
--
-- > s = Struct4 1 2 3 4
-- > $(acs 4 3) s
--
acs :: Int -> Int -> ExpQ
acs big_n small_n = [| \struct -> $(caseE [| struct |] [m]) |]
    where
          m :: MatchQ
          m = match pat (normalB $ varE $ vrs !! (small_n-1)) []

          pat :: PatQ
          pat = conP str $ map varP $ take big_n vrs

          str = mkName $ "Struct" ++ show big_n

          vrs = fieldnames ""

-- Templating functions

structTypeT :: Int -> Dec
#if __GLASGOW_HASKELL__ < 800
structTypeT nfields = DataD [] (structType nfields) tyVars [constructor] deriv''
#elif __GLASGOW_HASKELL__ < 802
structTypeT nfields = DataD [] (structType nfields) tyVars Nothing [constructor] deriv'
#else
structTypeT nfields = DataD [] (structType nfields) tyVars Nothing [constructor] [deriv]
#endif
    where
          tyVars    = map PlainTV $ take nfields $ fieldnames ""

          constructor = RecC (structType nfields) $ take nfields records

          records     = zipWith defRec (getters nfields) (fieldnames "")
#if __GLASGOW_HASKELL__ < 800
          defRec n t  = (,,) n NotStrict (VarT t)
#else
          defRec n t  = (,,) n (Bang NoSourceUnpackedness NoSourceStrictness) (VarT t)
#endif
          deriv'' = [''Show, ''Eq]

          deriv' = map ConT deriv''
#if __GLASGOW_HASKELL__ > 800
          deriv = DerivClause Nothing deriv'
#endif

storableInstanceT :: Int -> Dec
#if __GLASGOW_HASKELL__ < 800
storableInstanceT nfields = InstanceD cxt tp decs
#else
storableInstanceT nfields = InstanceD Nothing cxt tp decs
#endif
    where
          vars = take nfields $ fieldnames ""

          storable = AppT $ ConT ''Storable
#if __GLASGOW_HASKELL__ < 710
          cxt  = map (\v -> ClassP ''Storable [VarT v]) vars
#else
          cxt  = map (storable . VarT) vars
#endif
          tp   = storable $ foldl AppT (ConT $ structType nfields) $ map VarT vars

          decs = [ sizeOfT nfields
                 , alignmentT nfields
                 , peekT nfields
                 , pokeT nfields
                 ]

-- Storable instance function temaples

sizeOfT :: Int -> Dec
sizeOfT nfields = FunD 'sizeOf [clause]
    where
          clause = Clause [VarP struct] (NormalB body) wheres

          body = AppE (AppE (VarE 'sizeof) $ alignments "a") (sizes "s")

          alignments = ListE . take nfields . map VarE . fieldnames

          sizes = ListE . take nfields . map VarE . fieldnames

          wheres = vals 'alignment nfields "a" ++ vals 'sizeOf nfields "s"

alignmentT :: Int -> Dec
alignmentT nfields = FunD 'alignment [clause]
    where
           clause = Clause [VarP struct] (NormalB body) wheres

           body = AppE (VarE 'fmax) (ListE $ take nfields $ map VarE $ fieldnames "")

           wheres = vals 'alignment nfields ""

peekT :: Int -> Dec
peekT nfields = FunD 'peek [clause]
    where
          vars = take nfields $ fieldnames ""

          ptrs = tail $ take nfields $ fieldnames "_ptr"

          clause = Clause [VarP ptr] (NormalB body) []

          body = DoE $ initial ++ concat gotos ++ final

          initial = [ BindS (VarP $ head vars) (AppE (VarE 'peek) castPtr')
                    , BindS (VarP $ head ptrs) (AppE (AppE (VarE 'next) $ VarE ptr) $ VarE $ head vars)
                    ]


          gotos = zipWith3 goto (tail vars) ptrs (tail ptrs)

          goto n p next_p = [bindVar' p n, bindPtr' next_p p (VarE n)]

          final = [ bindVar' (last ptrs) (last vars)
                  , NoBindS $ AppE (VarE 'return) $ foldl AppE (ConE (structType nfields)) (map VarE vars)
                  ]

pokeT :: Int -> Dec
pokeT nfields = FunD 'poke [clause]
    where
          vars = take nfields $ fieldnames ""

          ptrs = tail $ take nfields $ fieldnames "_ptr"

          clause = Clause patterns (NormalB body) []

          patterns = [VarP ptr, ConP (structType nfields) (map VarP vars)]

          body = DoE $ [init_poke, init_next] ++ concat gotos ++ [final]

          init_poke = NoBindS
                    $ AppE cast_poke_ptr (VarE $ head vars)
                    where  cast_poke_ptr = AppE (VarE 'poke) castPtr'

          init_next = bindPtr' (head ptrs) ptr (VarE $ head vars)

          gotos = zipWith3 goto (tail vars) ptrs $ tail ptrs

          goto n p next_p = [pokeVar' p var, bindPtr' next_p p var]
                where var = VarE n

          final = pokeVar' (last ptrs) (VarE $ last vars)

-- Helpers and Constants

structType n = mkName ("Struct" ++ show n)

struct   = mkName "struct"

ptr      = mkName "ptr"

castPtr' = AppE (VarE 'castPtr) (VarE ptr)

fieldnames :: String -> [Name]
fieldnames s = map (mkName . (:s)) ['a'..'z']

getters    :: Int -> [Name]
getters n = map (mkName . (("s" ++ show n) ++))
          $  ["1st","2nd","3rd"]
          ++ [show n ++ "th" | n <- [4..]]

vals f n s = take n $ zipWith val (fieldnames s) (getters n)
    where
          val v getter = ValD (VarP v) (NormalB $ body getter) []

          body getter  = AppE (VarE f) $ AppE (VarE getter) $ VarE struct

bindVar' ptr var = BindS (VarP var) (AppE (VarE 'peek) $ VarE ptr)

pokeVar' ptr var = NoBindS
       $ AppE (AppE (VarE 'poke) $ VarE ptr) var

bindPtr' np pp var = BindS (VarP np)
       $ AppE next_ptr var
       where next_ptr = AppE (VarE 'next) $ VarE pp

