import Text.Read

-- MataU is the data of which currency is available

data MataU = USD 
            | EUR
            | JPY
            | IDR
        deriving (Eq)

-- This will show the symbol of the currency for later.
instance Show MataU where
  show USD = "$"
  show JPY = "¥"
  show EUR = "€"
  show IDR = "Rp"

-- cekada is to check if the currency input is correct or nothing.
cekada :: String -> Maybe MataU
cekada "EUR" = Just EUR
cekada "JPY" = Just JPY 
cekada "USD" = Just USD 
cekada "IDR" = Just IDR 
cekada _     = Nothing

-- list of the rate 
exchanges :: [((MataU, MataU), Float)]
exchanges = [
    ((EUR, EUR), 1),
    ((EUR, JPY), 128.14),
    ((EUR, USD), 1.10),
    ((EUR, IDR), 15750),
    ((JPY, EUR), 0.0078),
    ((JPY, JPY), 1),
    ((JPY, USD), 0.0086),
    ((JPY, IDR), 122.93),
    ((USD, EUR), 0.91),
    ((USD, JPY), 116.46),
    ((USD, USD), 1),
    ((USD, IDR), 14312),
    ((IDR, EUR), 0.000064),
    ((IDR, JPY), 0.0081),
    ((IDR, USD), 0.00007),
    ((IDR, IDR), 1)
            ]

-- look for "from" and "to" currency and get the rate from exchanges (above)            
findexchange :: MataU -> MataU -> Maybe Float 
findexchange dari jadi = lookup (dari, jadi) exchanges


-- This is to create the log file.
logger :: MataU -> MataU -> Float -> Float -> IO ()
logger dari jadi amount result = appendFile "log.txt" $
 "Current currency:" ++ show dari ++ ", Target currency:" ++ show jadi ++
 ", Initial amount:" ++ show amount ++ ", Resulting amount: " ++ show result ++ "\n"

-- This is the function start.  
main :: IO()
main = do 
  putStrLn "Your Original Currency (EUR/IDR/JPY/USD)"
  kursasal <- getLine
  print kursasal
  case cekada kursasal of 
    Nothing ->  putStrLn "Not available!" 
    (Just dari) -> do 
      putStrLn "\nHow much you want to exchange"
      jumlah <- getLine
      case readMaybe jumlah of 
        Nothing -> putStrLn "That's not a number!"
        Just n  -> do 
          putStrLn "What is your target Currency? (EUR/IDR/JPY/USD)"
          kursTujuan <- getLine
          case cekada kursTujuan of 
            Nothing -> putStrLn "Not available!"
            (Just jadi) -> case (findexchange dari jadi) of 
              Nothing -> putStrLn "exchange rate not available"
              (Just rate) -> do
                logger dari jadi n (rate * n) 
                putStrLn $ 
                 "Result: " ++ (show dari) ++ jumlah ++ " "  ++ "is equal to " ++ (show jadi) ++ " " ++ (show (rate * n)) 

      

      
  
        