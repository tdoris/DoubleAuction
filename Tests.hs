import DoubleAuction
import Test.QuickCheck
import Text.Printf
import Data.Maybe
import Data.List

instance Arbitrary TIF where
  arbitrary = elements [Day, IOC]

instance Arbitrary Side where
  arbitrary = elements [Buy, Sell]

instance Arbitrary Order where
  arbitrary = do
    side <- arbitrary
    size <- choose (0,100)
    price <- choose (0.0, 10.0)
    tif <- arbitrary
    return (Order side price size tif)

trades :: [Order] -> Order -> [Trade]
trades [] _ = []
trades (h:rest) o = if pricematch h o && matchqty > 0 then Trade matchqty (orderprice h) : trades rest (o {ordersize = ordersize o - matchqty}) else trades rest o
  where matchqty = min (ordersize h) (ordersize o)

orders :: [Order] -> Order -> [Order]
orders [] _ = []
orders os o | ordersize o == 0 = os
orders (h:rest) o = if pricematch h o && matchqty >0
                    then (if ordersize h - matchqty >0 then h { ordersize = ordersize h - matchqty } : orders rest (o {ordersize = ordersize o - matchqty}) else orders rest (o {ordersize = ordersize o - matchqty} ) )
                    else h : orders rest o
  where matchqty = min (ordersize h) (ordersize o)
  
residual :: [Order]-> Order -> Order
residual os o = o { ordersize = ordersize o - tradedqty }
  where tradedqty = sum (map tradesize (trades os o))
 
prop_pricematch_price o1 o2 = if orderside o1 /= orderside o2 && bprice >= sprice then pricematch o1 o2 else not (pricematch o1 o2)
                            where bprice = if orderside o1 == Buy then orderprice o1 else orderprice o2
                                  sprice = if orderside o1 == Sell then orderprice o1 else orderprice o2

prop_scanMatch_qty_invariant :: [Order]->Order->Bool
prop_scanMatch_qty_invariant os o = 
  case residual of 
    Nothing ->sumQty newbook +  (2 * sumTradeQty trades) == sumQty os + ordersize o
    Just r -> ordersize r  + sumQty newbook +  (2 * sumTradeQty trades) == sumQty os + ordersize o
  where (newbook,trades, residual) = scanMatch match os (Just o)
        sumQty = sum . map ordersize
        sumTradeQty = sum . map tradesize

prop_repeat_match_on_residual os o = residualA == residualB
  where (newbook, trades, residualA) = scanMatch match os o
        (_, _,residualB) = case residualA of 
                          Nothing -> ([], [], Nothing)
                          Just r -> scanMatch match newbook (Just r)

prop_order_count os o = after  <= before+1
  where before = length os
        after = length os'
        tradeCount = length ts
        (os', ts, residual) = scanMatch match os (Just o)

prop_trades_correct::[Order] -> Order -> Bool 
prop_trades_correct os o = trades os o == ts
  where (_,ts,_) = scanMatch match os (Just o)

prop_orders_correct::[Order] -> Order -> Bool 
prop_orders_correct os o = orders os o == os'
  where (os', _,_) = scanMatch match os (Just o)

prop_residual_correct::[Order] -> Order -> Bool 
prop_residual_correct os o =  (ordersize o - totalTraded ) == residual
  where totalTraded = sum (map tradesize (trades os o))
        (_,_,r) = scanMatch match os (Just o)
        residual = case r of
                    Nothing -> 0
                    Just x -> ordersize x

prop_add_correct :: [Order]->Order -> Bool
prop_add_correct os o = newbook == newbook' && ts == ts'
  where (newbook, ts) = add os o
        newbook' = if ordersize res > 0 && tif res == Day then insert res (orders os o) else orders os o
        ts' = trades os o
        res = residual os o

tests = [("pricematch", quickCheck prop_pricematch_price),
         ("qty_invariant", quickCheck prop_scanMatch_qty_invariant),
         ("match_on_residual", quickCheck prop_repeat_match_on_residual),
         ("prop_order_count", quickCheck prop_order_count),
         ("prop_trades_correct", quickCheck prop_trades_correct),
         ("prop_orders_correct", quickCheck prop_orders_correct),
         ("prop_residual_correct", quickCheck prop_residual_correct),
         ("prop_add_correct", quickCheck prop_add_correct)
        ]

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

