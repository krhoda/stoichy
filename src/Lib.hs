module Lib
    ( someFunc,
      lithium,
      getAmu,
      getPosCharge,
      getNegCharge,
      addElectron,
      removeElectron,
    ) where

import Data.Data
import Data.Maybe

data Particle = Electron | Neutron | Proton deriving (Show, Eq, Ord)
type Atom = (Valence, [Particle], [Particle])

toAmu :: Particle -> Maybe Int
toAmu Electron = Nothing
toAmu Neutron = Just 1
toAmu Proton = Just 1

toCount :: Particle -> Int
toCount _ = 1

getPosCharge :: Atom -> Int
getPosCharge (_, _, pt) = sum (map toCount pt)

getAmu :: Atom -> Int
getAmu (el, nt, pt) = sum (mapMaybe toAmu (nt ++ pt))

type Valence = (Filled, OuterShell)

type Filled = Int
type OuterShell = ([Particle], ValanceCapacity)

type ValanceCapacity = Int

getNegCharge :: Atom -> Int
getNegCharge ((filled, (outer, _)), _, _) = -((sum $ map toCount outer) + simpleValence filled)

addElectron :: Atom -> Int -> Atom
addElectron atom next
    | next <= 0 = atom
    | next + subj > getOuterCap atom = atom
    | otherwise = applyElec atom (next + subj)
    where subj = getOuterLen atom

removeElectron :: Atom -> Int -> Atom
removeElectron atom next
    | next <= 0 = atom
    | subj - next < 0 = applyElec atom 0
    | otherwise = applyElec atom (subj - next)
    where subj = getOuterLen atom

getOuterCap :: Atom -> Int
getOuterCap ((_, (_, x)), _, _) = x

getOuterLen :: Atom -> Int
getOuterLen ((_, (x, _)), _, _) = length x

applyElec :: Atom -> Int -> Atom
applyElec ((filled, (outer, cap)), pt, nt) next = ((filled, (replicate next Electron, cap)), pt, nt)

-- TODO: Make a real valence structure, moving all over the crazy thing.
simpleValence :: Int -> Int
simpleValence x
    | x <= 0 = 0
    | x == 1 = 2
    | x == 2 = 8
    | x == 3 = 18
    | x == 4 = 32
    | x == 5 = 32
    | x == 6 = 18
    | x == 7 = 8
    | otherwise = 0


-- buildAtom :: Int -> Int -> Atom

lithium :: Atom
lithium = ((1, ([Electron, Electron], simpleValence 2)), [Proton, Proton], [Neutron, Neutron])

someFunc :: IO ()
someFunc = print (show (getAmu lithium))
