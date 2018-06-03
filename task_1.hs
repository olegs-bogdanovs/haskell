data VatType = Default | Preferential
data ProductType = PType VatType Double

productprice :: ProductType -> Double
productprice(PType Default x) = x + x * 0.12
productprice(PType Preferential x) = x + x * 0.21

totalprice :: [ProductType] -> Double
totalprice lst = sum $ map productprice lst

