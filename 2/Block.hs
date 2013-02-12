{-# LANGUAGE OverloadedStrings #-}

import Data.Char (ord, chr)
import Data.Bits (xor, (.&.))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Crypto.Cipher.AES (encryptECB, decryptECB, initKey)

import Data.ByteString.Base16 (decode)

type Block = ByteString
type IV = ByteString
type Key = ByteString

blockBytes :: Int
blockBytes = 16

encrypt :: Key -> Block -> Block
encrypt key = encryptECB (initKey key)

decrypt :: Key -> Block -> Block
decrypt key = decryptECB (initKey key)

bsxor :: ByteString -> ByteString -> ByteString
bsxor xs ys = BS.pack $ BS.zipWith cxor xs ys
  where cxor x y = chr $ (ord x) `xor` (ord y)

cbc :: Key -> IV -> ByteString -> ByteString
cbc key iv msg = BS.concat $ iv : go iv (paddedBlocks msg)
  where go _ [] = []
        go iv (b : bs) = c : go c bs
          where c = encrypt key (iv `bsxor` b)

uncbc :: Key -> ByteString -> ByteString
uncbc key encrypted = BS.concat $ go iv (blocks encrypted')
  where (iv, encrypted') = BS.splitAt blockBytes encrypted

        go iv [b] = [unpad $ iv `bsxor` decrypt key b]
        go iv (b : bs) = iv `bsxor` decrypt key b : go b bs

inc :: IV -> IV
inc = BS.reverse . go . BS.reverse
  where go s | BS.null s = ""
             | otherwise =
               let Just (h, t) = BS.uncons s
                   h' = ord h + 1 .&. 0xff
               in BS.cons (chr h') (if h' == 0 then go t else t)

ctr :: Key -> IV -> ByteString -> ByteString
ctr key iv msg = BS.concat $ iv : go iv (blocks msg)
  where go _ [] = []
        go iv (b : bs) = encrypt key iv `bsxor` b : go (inc iv) bs

unctr :: Key -> ByteString -> ByteString
unctr key encrypted = BS.concat $ go iv (blocks encrypted')
 where (iv, encrypted') = BS.splitAt blockBytes encrypted

       go _ [] = []
       go iv (b : bs) = encrypt key iv `bsxor` b : go (inc iv) bs

pad :: Block -> Block
pad b = BS.concat [b, BS.replicate n (chr n)]
  where n = blockBytes - BS.length b

unpad :: Block -> Block
unpad b = BS.take n b
  where padLength = fromIntegral $ ord $ BS.last b
        n = BS.length b - padLength

paddedBlocks :: ByteString -> [Block]
paddedBlocks s
  | BS.length s < blockBytes = [pad s]
  | otherwise                = let (s', rest) = BS.splitAt blockBytes s
                               in s' : paddedBlocks rest

blocks :: ByteString -> [Block]
blocks s
  | BS.null s = []
  | otherwise = let (s', rest) = BS.splitAt blockBytes s
                in s' : blocks rest

unhex :: ByteString -> ByteString
unhex = fst . decode

q1 :: ByteString
q1 = uncbc key msg
  where key = unhex "140b41b22a29beb4061bda66b6747e14"
        msg = unhex "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee\
                    \2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"

q2 :: ByteString
q2 = uncbc key msg
  where key = unhex "140b41b22a29beb4061bda66b6747e14"
        msg = unhex "5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48\
                    \e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253"

q3 :: ByteString
q3 = unctr key msg
  where key = unhex "36f18357be4dbd77f050515c73fcf9f2"
        msg = unhex "69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc3\
\88d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329"

q4 :: ByteString
q4 = unctr key msg
  where key = unhex "36f18357be4dbd77f050515c73fcf9f2"
        msg = unhex "770b80259ec33beb2561358a9f2dc617e46218c0a53cbeca695ae45faa8952aa\
\0e311bde9d4e01726d3184c34451"
