{-# LANGUAGE TemplateHaskell #-}
module Foreign.C.Structs.Templates
    (structT)
where

import Language.Haskell.TH
import Data.List (zipWith4)

import Foreign.Storable (Storable, peek, poke, sizeOf, alignment)
import Foreign.Ptr (castPtr)
import Foreign.C.Structs.Utils (next, sizeof, fmax)

structT :: Int -> DecsQ
structT = return . zipWith ($) [structTypeT, storableInstanceT] . repeat

sTypeN n = mkName ("Struct" ++ show n)
struct   = mkName "struct"
ptr      = mkName "ptr"

fieldnames :: String -> [Name]
fieldnames s = map (mkName . (:s)) ['a'..'z']
getters    :: Int -> [Name]
getters n = map (mkName . (("s" ++ show n) ++))
          $  ["fst","snd","trd","fth","fif","sxt","sth","egt","nth"]
          ++ [show n ++ "th" | n <- [10..]]

vals f n s = take n $ zipWith val (fieldnames s) (getters n)
    where val v getter = ValD (VarP v) (NormalB $ body getter) []
          body getter  = AppE (VarE f) $ AppE (VarE getter) $ VarE struct

sizeOfT :: Int -> Dec
sizeOfT nfields = FunD 'sizeOf [clause]
    where clause = Clause [VarP struct] (NormalB body) wheres
          body = AppE (AppE (VarE 'sizeof) $ alignments "a") (sizes "s")
          alignments = ListE . take nfields . map VarE . fieldnames
          sizes = ListE . take nfields . map VarE . fieldnames
          wheres = vals 'alignment nfields "a" ++ vals 'sizeOf nfields "s"

alignmentT :: Int -> Dec
alignmentT nfields = FunD 'alignment [clause]
     where clause = Clause [VarP struct] (NormalB body) wheres
           body = AppE (VarE 'fmax) (ListE $ take nfields $ map VarE $ fieldnames "")
           wheres = vals 'alignment nfields ""

peekT :: Int -> Dec
peekT nfields = FunD 'peek [clause]
    where
          vars = take nfields $ fieldnames ""
          ptrs = tail $ take nfields $ fieldnames "_ptr"
          clause = Clause [VarP ptr] (NormalB body) []

          body = DoE $ initial ++ concat gotos ++ final
          initial = [ BindS (VarP $ head vars) (AppE (VarE 'peek) (AppE (VarE 'Foreign.Ptr.castPtr) (VarE ptr)))
                    , BindS (VarP $ head ptrs) (AppE (AppE (VarE 'next) $ VarE ptr) $ VarE $ head vars)
                  ]
          gotos = zipWith3 goto (tail vars) (ptrs) (tail ptrs)
          goto n p next_p = [bind_var, bind_ptr]
                where bind_var = BindS (VarP n) (AppE (VarE 'peek) $ VarE p)
                      bind_ptr = BindS (VarP next_p) (AppE (AppE (VarE 'next) $ VarE p) $ VarE n)

          final = [ BindS (VarP $ last vars) (AppE (VarE 'peek) $ VarE $ last ptrs)
                  , NoBindS $ AppE (VarE 'return) $ foldl AppE (ConE (sTypeN nfields)) (map VarE vars)
                ]

pokeT :: Int -> Dec
pokeT nfields = FunD 'poke [clause]
    where
          vars = take nfields $ fieldnames ""
          ptrs = tail $ take nfields $ fieldnames "_ptr"
          clause = Clause patterns (NormalB body) []

          patterns = [VarP ptr, ConP (sTypeN nfields) (map VarP vars)]
          body = DoE $ [init_poke, init_next] ++ concat gotos ++ [final]
          init_poke = NoBindS $ AppE (AppE (VarE 'poke) $ AppE (VarE 'Foreign.Ptr.castPtr) $ VarE ptr) $ VarE $ head vars
          init_next = BindS (VarP $ head ptrs) (AppE (AppE (VarE 'next) $ VarE ptr) $ VarE $ head vars)
          final = NoBindS $ AppE (AppE (VarE 'poke) $ VarE $ last ptrs) $ VarE $ last vars

          gotos = zipWith3 goto (tail vars) (ptrs) $ tail ptrs
          goto n p next_p = [poke_var, bind_ptr]
                where var = VarE n
                      poke_var = NoBindS $ AppE (AppE (VarE 'poke) $ VarE p) var
                      bind_ptr = BindS (VarP next_p) $ AppE (AppE (VarE 'next) $ VarE p) var

structTypeT :: Int -> Dec
structTypeT nfields = DataD [] (sTypeN nfields) tyVars Nothing [constructor] [deriv]
    where tyVars    = map PlainTV $ take nfields $ fieldnames ""
          constructor = RecC (sTypeN nfields) $ take nfields $ records
          records     = zipWith defRec (getters nfields) (fieldnames "")
          defRec n t  = (,,) n (Bang NoSourceUnpackedness NoSourceStrictness) (VarT t)
          deriv = DerivClause Nothing [ConT ''Show, ConT ''Eq]

storableInstanceT :: Int -> Dec
storableInstanceT nfields = InstanceD Nothing cxt tp decs
    where vars = take nfields $ fieldnames ""
          storable = AppT $ ConT ''Storable

          cxt  = map (storable . VarT) vars
          tp   = storable $ foldl AppT (ConT $ sTypeN nfields) $ map VarT vars

          decs = [ sizeOfT nfields
                 , alignmentT nfields
                 , peekT nfields
                 , pokeT nfields
                 ]

