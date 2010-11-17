module DoubleAuction( add,insertInBook, match, scanMatch, pricematch, Order(..), Side(..), Trade(..), TIF(..)) where

import Data.Maybe

data Side = Buy | Sell deriving (Show,Eq)
data TIF = Day | IOC deriving (Show, Eq)

data Order = Order {orderside :: Side, ordersize :: Integer, orderprice :: Double, tif::TIF} deriving (Show, Eq)
data Trade = Trade {tradesize::Integer, tradeprice::Double} deriving (Show,Eq)

instance Ord Order where
  compare o1 o2 | orderside o1 == Buy && orderside o2 == Sell = LT
  compare o1 o2 | orderside o1 == Sell && orderside o2 == Buy = GT
  compare o1 o2 | orderside o1 == Buy && orderside o2 == Buy = compare (orderprice o1) (orderprice o2)
  compare o1 o2 | orderside o1 == Sell && orderside o2 == Sell = compare (orderprice o2) (orderprice o1)

add :: [Order] -> Order -> ([Order], [Trade])
add os o | ordersize o == 0 = (os, [])
add os o = 
  case residual of 
    Nothing -> (os', ts) 
    Just r -> if tif r == IOC then (os', ts) else ((insertInBook os' r), ts) 
  where (os', ts, residual) = scanMatch' match os (Just o)

insertInBook :: [Order] -> Order -> [Order]
insertInBook [] o = [o]
insertInBook os@(h:r) o = if o < h then o:os else h : insertInBook r o

scanMatch :: (Order -> Order -> (Maybe Order, Maybe Order, Maybe Trade)) -> [Order]->Maybe Order->([(Maybe Order, Maybe Trade)], Maybe Order)
scanMatch _ [] (Just o) = ([], Just o)
scanMatch _ os Nothing = (zip (map Just os) (repeat Nothing) , Nothing)
scanMatch f (h:os) (Just o) = ((resB, t) : fmL, fmo)
  where (resB, resO, t) = f h o
        (fmL, fmo) = scanMatch f os resO

scanMatch' :: (Order -> Order -> (Maybe Order, Maybe Order, Maybe Trade)) -> [Order]->Maybe Order->([Order], [Trade], Maybe Order)
scanMatch' _ [] (Just o) = ([],[], Just o)
scanMatch' _ os Nothing = (os,[], Nothing)
scanMatch' f (h:os) (Just o) = (newbook, trades, residual)
  where newbook = if isNothing bookOrderResidual then fos else fromJust bookOrderResidual : fos
        trades = if isNothing thisTrade then ftrades else fromJust thisTrade : ftrades
        (bookOrderResidual, newOrderResidual, thisTrade) = f h o
        (fos, ftrades, residual) = scanMatch' f os newOrderResidual

-- match two orders, return residuals and trade
match:: Order -> Order -> (Maybe Order, Maybe Order, Maybe Trade)
match o1 o2 = (resid o1, resid o2, t)
  where matchqty = min (ordersize o1) (ordersize o2)
        t = if not matches then Nothing else Just (Trade matchqty (orderprice o1))
        resid order = if not matches then Just order else (if ordersize order == matchqty then Nothing else Just order { ordersize = ordersize order - matchqty } )
        matches = pricematch o1 o2 && matchqty >0

pricematch :: Order->Order->Bool
pricematch o1 o2 | orderside o1 == orderside o2 = False
pricematch (Order side1 _ price1 _) (Order side2 _ price2 _) = sellprice <= buyprice
  where buyprice | side1 == Buy = price1
                 | side2 == Buy = price2
        sellprice | side1 == Sell = price1
                  | side2 == Sell = price2

