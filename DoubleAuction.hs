module DoubleAuction( add, match, scanMatch, pricematch, Order(..), Side(..), Trade(..), TIF(..)) where

import Data.List

data Side = Buy | Sell deriving (Show,Eq, Ord)
data TIF = Day | IOC deriving (Show, Eq, Ord)

data Order = Order {orderside :: Side, orderprice :: Double, ordersize :: Integer, tif::TIF} deriving (Show, Eq, Ord)
data Trade = Trade {tradesize::Integer, tradeprice::Double} deriving (Show,Eq)

add :: [Order] -> Order -> ([Order], [Trade])
add os o | ordersize o == 0 = (os, [])
add os o = 
  case residual of 
    Nothing -> (os', ts) 
    Just r -> if tif r == IOC then (os', ts) else (insert r os', ts) 
  where (os', ts, residual) = scanMatch match os (Just o)

scanMatch :: (Order -> Order -> (Maybe Order, Maybe Order, Maybe Trade)) -> [Order]->Maybe Order->([Order], [Trade], Maybe Order)
scanMatch _ [] (Just o) = ([],[], Just o)
scanMatch _ os Nothing = (os,[], Nothing)
scanMatch f (h:os) (Just o) = (newbook, trades, residual)
  where newbook = maybeCons bookOrderResidual fos
        trades = maybeCons thisTrade ftrades
        (bookOrderResidual, newOrderResidual, thisTrade) = f h o
        (fos, ftrades, residual) = scanMatch f os newOrderResidual

maybeCons :: Maybe a -> [a] -> [a]
maybeCons (Just x) xs = x : xs
maybeCons Nothing xs = xs

match:: Order -> Order -> (Maybe Order, Maybe Order, Maybe Trade)
match o1 o2 = (residual o1, residual o2, t)
  where matchqty = min (ordersize o1) (ordersize o2)
        t = if not matches then Nothing else Just (Trade matchqty (orderprice o1))
        residual order = if not matches then Just order else (if ordersize order == matchqty then Nothing else Just order { ordersize = ordersize order - matchqty } )
        matches = pricematch o1 o2 && matchqty >0

pricematch :: Order->Order->Bool
pricematch o1 o2 = pm o1 o2 || pm o2 o1
  where pm x y = orderside x == Buy && orderside y == Sell && orderprice x >= orderprice y

