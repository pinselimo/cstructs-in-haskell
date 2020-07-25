{-# LANGUAGE TemplateHaskell #-}
module Foreign.C.Structs.Templates
    (structT)
where

import Language.Haskell.TH

import Foreign.Storable (Storable, peek, poke, sizeOf, alignment)
import Foreign.Ptr (castPtr)
import Foreign.C.Structs.Utils (next, sizeof, fmax)

structT :: Int -> DecsQ
structT = return . zipWith ($) [structTypeT, storableInstanceT] . repeat

-- Templating functions

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

-- Storable instance function temaples

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
          initial = [ BindS (VarP $ head vars) (AppE (VarE 'peek) cast_ptr)
                    , BindS (VarP $ head ptrs) (AppE (AppE (VarE 'next) $ VarE ptr) $ VarE $ head vars)
                  ]
          gotos = zipWith3 goto (tail vars) (ptrs) (tail ptrs)
          goto n p next_p = [bind_var p n, bind_ptr next_p p (VarE n)]

          final = [ bind_var (last ptrs) (last vars)
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

          init_poke = NoBindS
                    $ AppE cast_poke_ptr (VarE $ head vars)
                    where  cast_poke_ptr = AppE (VarE 'poke) cast_ptr
          init_next = bind_ptr (head ptrs) ptr (VarE $ head vars)

          gotos = zipWith3 goto (tail vars) (ptrs) $ tail ptrs
          goto n p next_p = [poke_var p var, bind_ptr next_p p var]
                where var = VarE n

          final = poke_var (last ptrs) (VarE $ last vars)

-- Helpers and Constants

sTypeN n = mkName ("Struct" ++ show n)
struct   = mkName "struct"
ptr      = mkName "ptr"
cast_ptr = AppE (VarE 'castPtr) (VarE ptr)

fieldnames :: String -> [Name]
fieldnames s = map (mkName . (:s)) ['a'..'z']
getters    :: Int -> [Name]
getters n = map (mkName . (("s" ++ show n) ++))
          $  ["fst","snd","trd","fth","fif","sxt","sth","egt","nth"]
          ++ [show n ++ "th" | n <- [10..]]

vals f n s = take n $ zipWith val (fieldnames s) (getters n)
    where val v getter = ValD (VarP v) (NormalB $ body getter) []
          body getter  = AppE (VarE f) $ AppE (VarE getter) $ VarE struct

bind_var ptr var = BindS (VarP var) (AppE (VarE 'peek) $ VarE ptr)
poke_var ptr var = NoBindS
       $ AppE (AppE (VarE 'poke) $ VarE ptr) var
bind_ptr np pp var = BindS (VarP np)
       $ AppE next_ptr var
       where next_ptr = AppE (VarE 'next) $ VarE pp

