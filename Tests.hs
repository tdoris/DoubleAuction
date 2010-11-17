import DoubleAuction
import Test.QuickCheck
import Text.Printf

instance Arbitrary Side where
  arbitrary = elements [Buy, Sell]

instance Arbitrary Order where
  arbitrary = do
    side <- arbitrary
    size <- choose (0,100)
    price <- choose (0.0, 10.0)
    return (Order side size price)

instance Arbitrary Book where
  arbitrary = do 
    orders <- orderedList
    return (Book orders)


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
   
prop_pricematch_price o1 o2 = if orderside o1 /= orderside o2 && bprice >= sprice then pricematch o1 o2 == True else pricematch o1 o2== False
                            where bprice = if orderside o1 == Buy then orderprice o1 else orderprice o2
                                  sprice = if orderside o1 == Sell then orderprice o1 else orderprice o2

prop_scanMatch_qty_invariant :: [Order]->Order->Bool
prop_scanMatch_qty_invariant os o = 
  case residual of 
    Nothing ->sumQty newbook +  (2 * sumTradeQty trades) == sumQty os + ordersize o
    Just r -> ordersize r  + sumQty newbook +  (2 * sumTradeQty trades) == sumQty os + ordersize o
  where (osts, residual) = scanMatch match os (Just o)
        newbook = extract $ map fst osts
        trades = extract (map snd osts)
        sumQty orders = sum (map ordersize orders)
        sumTradeQty ts = sum (map tradesize ts)

prop_repeat_match_on_residual os o = residualA == residualB
  where (osts, residualA) = scanMatch match os o
        (_, residualB) = case residualA of 
                          Nothing -> ([], Nothing)
                          Just r -> scanMatch match residualOrders (Just r)
        residualOrders = extract $ map fst osts

prop_order_count os o = after  <= before+1
  where before = length os
        after = length os'
        os' = extract $ map fst osts
        tradeCount = (length . extract) (map snd osts)
        (osts, residual) = scanMatch match os (Just o)

prop_trades_correct::[Order] -> Order -> Bool 
prop_trades_correct os o = trades os o == extract (map snd (fst (scanMatch match os (Just o)))) 

prop_orders_correct::[Order] -> Order -> Bool 
prop_orders_correct os o = orders os o == extract (map fst (fst (scanMatch match os (Just o)))) 

prop_residual_correct::[Order] -> Order -> Bool 
prop_residual_correct os o =  (ordersize o - totalTraded ) == residual
  where totalTraded = sum (map tradesize (trades os o))
        residual = case snd (scanMatch match os (Just o)) of
                    Nothing -> 0
                    Just x -> ordersize x

tests = [("pricematch", quickCheck prop_pricematch_price),
         ("qty_invariant", quickCheck prop_scanMatch_qty_invariant),
         ("match_on_residual", quickCheck prop_repeat_match_on_residual),
         ("prop_order_count", quickCheck prop_order_count),
         ("prop_trades_correct", quickCheck prop_trades_correct),
         ("prop_orders_correct", quickCheck prop_orders_correct),
         ("prop_residual_correct", quickCheck prop_residual_correct)
        ]

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

