{-
To run this program, first install GHC.
Then run:
ghc --make solution.hs
./solution.hs < encrypted.txt

I first tried to solve part one, with a list comprehension like:
partOne :: (Int, Int, Int)
partOne = head [(x, y, z) | z <- [1..], x <- [1..z], y <- [x..z], 
                                   x^2 + y^2 == z^2,
                                   x + y + z == 4070]

However, I quickly realized the search space is too big.

Some Googling revealed all pythagorean triplets are of the form:
a = 2*m*n
b = m^2 - n^2
c = m^2 + n^2
where m > n > 0
          
a + b + c = 4070 implies that
m^2 + n^2 +  m^2 - n^2 + 2*m*n = 4070
After some cleaning up:
m * (m+n) = 2035

Because m > n, we know
m^2 < m * (m+n) < 2035
and
2m^2 > m * (m+n) which implies
m^2 > 1017.5

Therefore, we know:
31.89 < m < 45.12

Since both sides of m^2 + n*m = 2035 must yield integers when divided by m, we know that m must be a factor of 2035
The prime factorization of 2035 is 5*11*37 = 2035, so the only
possible value of m is 37, and elementary algebra shows n = 18

Therefore,
a = 1332
b = 1045
c = 1693

w is  52 or 21. However, 52 is not coprime to 256 so:
w = 21
v = 157
-}

import Data.Word
import Data.Char
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Key = Key {
      w :: Word8
    , v :: Word8
    } deriving Show
           

encode :: Key -> Word8 -> Word8
encode k x = x * (w k) + (v k) `mod` 256

decode :: Map.Map Word8 Word8 -> Word8 -> Char
decode m e = chr $ fromIntegral (Maybe.fromJust $ Map.lookup e m)

--I'll just be lazy and just create a 'rainbow table' instead
--of properly figuring out the inverse of the encryption fn
generateDecodeMapHelper :: Key -> Word8 -> [(Word8, Word8)]
generateDecodeMapHelper _ 0 = []
generateDecodeMapHelper k n = (encode k n, n) : generateDecodeMapHelper k (n-1)

generateDecodeMap :: Key -> Map.Map Word8 Word8
generateDecodeMap k = Map.fromList $ generateDecodeMapHelper k 255

partTwo :: Key -> B.ByteString -> [Char]
partTwo k bs = map decoder (B.unpack bs) where 
    decoder = decode $ generateDecodeMap k

main :: IO ()
main = do 
  contents <- B.getContents
  putStrLn $ show (generateDecodeMap Key {w = 21, v = 157})
--  putStrLn $ partTwo Key {w = 21, v = 157} contents