module DoubleAuction( match, extract, scanMatch, pricematch, Order(..), Side(..), Trade(..), Book(..)) where

data Side = Buy | Sell deriving (Show,Eq)

data Order = Order {orderside :: Side, ordersize :: Integer, orderprice :: Double} deriving (Show, Eq)
data Trade = Trade {tradesize::Integer, tradeprice::Double} deriving (Show,Eq)
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

intract :: [a] -> [Maybe a]
intract [] = []
intract (x:xs) = Just x : intract xs

add :: Book -> Order -> (Book, [Trade])
add (Book os) o = 
  case residual of 
    Nothing -> (Book os', ts) 
    Just r -> (Book (insertInBook os' r), ts) 
  where (osts, residual) = scanMatch match os (Just o)
        os' = extract $ map fst osts
        ts = extract $ map snd osts

insertInBook :: [Order] -> Order -> [Order]
insertInBook [] o = [o]
insertInBook os@(h:r) o = if o < h then o:os else h : insertInBook r o

scanMatch :: (Order -> Order -> (Maybe Order, Maybe Order, Maybe Trade)) -> [Order]->Maybe Order->([(Maybe Order, Maybe Trade)], Maybe Order)
scanMatch _ [] (Just o) = ([], Just o)
scanMatch _ os Nothing = (zip (intract os) (repeat Nothing) , Nothing)
scanMatch f (h:os) (Just o) = ((resB, t) : fmL, fmo)
  where (resB, resO, t) = match h o
        (fmL, fmo) = scanMatch f os resO

-- match two orders, return residuals and trade
match:: Order -> Order -> (Maybe Order, Maybe Order, Maybe Trade)
match o1 o2 = (resid o1, resid o2, t)
  where matchqty = min (ordersize o1) (ordersize o2)
        t = if not matches then Nothing else Just (Trade matchqty (orderprice o1))
        resid order = if not matches then Just order else (if ordersize order == matchqty then Nothing else Just order { ordersize = ordersize order - matchqty } )
        matches = pricematch o1 o2 && matchqty >0

pricematch :: Order->Order->Bool
pricematch (Order side1 _ _) (Order side2 _ _) | side1 == side2  = False
pricematch (Order side1 _ price1) (Order side2 _ price2) = sellprice <= buyprice
  where buyprice | side1 == Buy = price1
                 | side2 == Buy = price2
        sellprice | side1 == Sell = price1
                  | side2 == Sell = price2

