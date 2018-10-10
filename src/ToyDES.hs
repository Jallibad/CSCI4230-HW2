{-# LANGUAGE BinaryLiterals		#-}
{-# LANGUAGE DataKinds			#-}
{-# LANGUAGE FlexibleContexts	#-}
{-# LANGUAGE LambdaCase			#-}
{-# LANGUAGE OverloadedStrings	#-}
{-# LANGUAGE TypeApplications	#-}
{-# LANGUAGE TypeFamilies		#-}
{-# LANGUAGE TypeOperators		#-}
{-# LANGUAGE ViewPatterns		#-}

module ToyDES where

import BitVector
import Control.Arrow ((***),(&&&))
import Data.Bits (shift, xor)
import Data.ByteString (ByteString, readFile, writeFile)
import qualified Data.ByteString as ByteString (map)
import Data.List (genericIndex)
import Data.Tuple (swap)
import Prelude hiding (readFile, writeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

--main = print $ encrypt 0b10110101 0b1110001110
main = getArgs >>= \case
		[mode, (fromInteger . read -> key), inputFile, outputFile] -> do
			input <- readFile inputFile
			let operation = case mode of
				"e" -> encrypt'
				"d" -> decrypt'
			writeFile outputFile $ operation key input
			exitWith ExitSuccess
		_ -> putStrLn "Invalid Arguments" >> exitWith (ExitFailure 1)

encrypt' :: BitVector 10 -> ByteString -> ByteString
encrypt' k = ByteString.map $ fromIntegral . flip encrypt k . fromIntegral

decrypt' :: BitVector 10 -> ByteString -> ByteString
decrypt' k = ByteString.map $ fromIntegral . flip decrypt k . fromIntegral

encrypt :: BitVector 8 -> BitVector 10 -> BitVector 8
encrypt p = inversePerm			-- Flip and recombine l and r
			. foldl feistel		-- Apply the feistel function for each of the keys
				(initialPerm p)	-- Initial value to fold: plaintext block split in half
			. makeKeys			-- Returns a rotating list of keys [0..length of DES cycle]

decrypt :: BitVector 8 -> BitVector 10 -> BitVector 8
decrypt p =	inversePerm				-- Flip and recombine l and r
			. foldr (flip feistel)	-- Apply the feistel function for each of the keys
				(initialPerm p)		-- Initial value to fold: plaintext block split in half
			. makeKeys				-- Returns a rotating list of keys [0..length of DES cycle]

initialPerm :: BitVector 8 -> (BitVector 4, BitVector 4)
initialPerm = backPermute' [1,5,2,0] &&& backPermute' [3,7,4,6]

inversePerm :: (BitVector 4, BitVector 4) -> BitVector 8
inversePerm = backPermute' [7,4,6,0,2,5,3,1] . BitVector.concat'

expansion :: BitVector 4 -> BitVector 8
expansion = backPermute' [3,0,1,2,1,2,3,0]

f :: BitVector 4 -> BitVector 8 -> BitVector 4
f r =	backPermute' [1,3,2,0]	-- Apply P4 permutation
		. BitVector.concat'		-- Concat the output of the s-boxes
		. (s0 *** s1)			-- Use the s-boxes
		. BitVector.split		-- Split the result into s-box inputs
		. xor (expansion r)		-- Expand block then XOR with key
	where
		box :: [[BitVector 2]] -> BitVector 4 -> BitVector 2
		box s i = box' [1,2] $ box' [0,3] s
			where
				box' :: [Int] -> [a] -> a
				box' p = flip genericIndex $ backPermute' @4 @2 p i
		s0 = box [[1,0,3,2],[3,2,1,0],[0,2,1,3],[3,1,3,2]]
		s1 = box [[0,1,2,3],[2,0,1,3],[3,0,1,0],[2,1,0,3]]

makeKeys :: BitVector 10 -> [BitVector 8]
makeKeys =	take 2 . tail					-- Take the result of applying the operations 1x and 2x
			. map (p8 . BitVector.concat')	-- Use the provided p8 permutation to make key from halves
			. iterate (lShift *** lShift)	-- Repeatedly left shift both key halves
			. BitVector.split . p10			-- Apply initial p10 permutation and then split key in half
	where
		p10 = backPermute' @10 @10 [2,4,1,6,3,9,0,8,7,5]
		p8 = backPermute' @10 @8 [5,2,6,3,7,4,9,8]
		lShift :: BitVector 5 -> BitVector 5
		lShift = flip shift 1

feistel :: (BitVector 4, BitVector 4) -> BitVector 8 -> (BitVector 4, BitVector 4)
feistel (l,r) k = (r, l `xor` f r k)

test n = decrypt (encrypt n k) k == n
		where k = 0b1110001110