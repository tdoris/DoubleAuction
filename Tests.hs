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


prop_doesmatch_price o1 o2 = if orderside o1 /= orderside o2 && bprice >= sprice then doesmatch o1 o2 == True else doesmatch o1 o2== False
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

tests = [("doesmatch", quickCheck prop_doesmatch_price),
         ("qty_invariant", quickCheck prop_scanMatch_qty_invariant),
         ("match_on_residual", quickCheck prop_repeat_match_on_residual),
         ("prop_order_count", quickCheck prop_order_count)
        ]

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

