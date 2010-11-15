module DoubleAuction( match, extract, scanMatch, doesmatch, Order(..), Side(..), Trade(..), Book(..)) where

data Side = Buy | Sell deriving (Show,Eq)

data Order = Order {orderside :: Side, ordersize :: Integer, orderprice :: Double} deriving (Show, Eq)
data Trade = Trade {tradesize::Integer, tradeprice::Double} deriving Show
data Book = Book [Order] deriving Show

instance Ord Order where
  compare o1 o2 | orderside o1 == Buy && orderside o2 == Sell = LT
  compare o1 o2 | orderside o1 == Sell && orderside o2 == Buy = GT
  compare o1 o2 | orderside o1 == Buy && orderside o2 == Buy = compare (orderprice o1) (orderprice o2)
  compare o1 o2 | orderside o1 == Sell && orderside o2 == Sell = compare (orderprice o2) (orderprice o1)

extract :: [Maybe a] -> [a]
extract [] = []
extract ((Just x):t) = x : extract t
extract (_:t) = extract t

add :: Book -> Order -> (Book, [Trade])
add (Book os) o = 
  case residual of 
    Nothing -> (Book os', ts) 
    Just r -> (Book (insertInBook os' r), ts) 
  where (osts, residual) = scanMatch match os o
        os' = map fst osts
        ts = extract $ map snd osts

insertInBook :: [Order] -> Order -> [Order]
insertInBook [] o = [o]
insertInBook os@(h:r) o = if o < h then o:os else h : insertInBook r o

scanMatch :: (Order -> Order -> (Order, Order, Maybe Trade)) -> [Order]->Order->([(Order, Maybe Trade)], Maybe Order)
scanMatch _ [] o | ordersize o == 0 = ([], Nothing)
scanMatch _ [] o | ordersize o > 0 = ([], Just o)
scanMatch f (h:os) o = ((resB, t) : fmL, fmo)
  where (resB, resO, t) = match h o
        (fmL, fmo) = scanMatch f os resO

match:: Order -> Order -> (Order, Order, Maybe Trade)
match o1 o2 = if not (doesmatch o1 o2) then (o1, o2, Nothing) 
              else ( o1 { ordersize = (ordersize o1 - matchqty)}, o2 { ordersize = ordersize o2 - matchqty }, Just (Trade matchqty (orderprice o1)))
  where matchqty = min (ordersize o1) (ordersize o2)

doesmatch :: Order->Order->Bool
doesmatch (Order side1 _ _) (Order side2 _ _) | side1 == side2  = False
doesmatch (Order side1 _ price1) (Order side2 _ price2) = sellprice <= buyprice
  where buyprice | side1 == Buy = price1
                 | side2 == Buy = price2
        sellprice | side1 == Sell = price1
                  | side2 == Sell = price2

