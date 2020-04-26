module DirectoryEngine
(
    DirectoryItem(..),
    mMkdir,
    mRmdir,
    mNewFile
)where 

import qualified Data.ByteString.Char8 as B
import Data.List
import Foreign.C.Error

data DirectoryItem = Directory
    {   dirs :: [(FilePath, DirectoryItem)]
    ,   files:: [FilePath]
    }
    deriving (Show, Read)

emptyDirectory :: DirectoryItem
emptyDirectory = Directory { dirs = [], files = []}

mMkdir :: [FilePath] -> DirectoryItem -> Either Errno DirectoryItem
mMkdir [] _= Left eINVAL
mMkdir [x] item 
    | x `elem` map fst (dirs item) = Left eEXIST
    | x `elem` files item = Left eEXIST
    | otherwise = Right $ item {
        dirs = (x,Directory{ dirs = [], files = []}):dirs item
    }
mMkdir (x:xs) item = 
    case x `elemIndex` map fst (dirs item) of 
        Nothing -> Left eINVAL
        Just n -> fmap (\it -> Directory {
            dirs  = take n (dirs item) ++ [(x,it)] ++ drop (n+1) (dirs item)
        ,   files = files item
        }) (mMkdir xs $ snd (dirs item!!n))

mRmdir :: [FilePath] -> DirectoryItem -> Either Errno DirectoryItem
mRmdir [] _ = Left eINVAL
mRmdir [x] item =
    case x `elemIndex` map fst (dirs item) of
        Nothing -> Left eNOENT
        Just n -> Right $ item {
            dirs = take n (dirs item) ++ drop (n+1) (dirs item)
        }

mRmdir (x:xs) item = 
    case x `elemIndex` map fst (dirs item) of
        Nothing -> Left eNOENT
        Just n -> fmap (\it -> Directory {
            dirs = take n (dirs item) ++ [(x,it)] ++ drop (n+1) (dirs item)
        ,   files = files item
        }) (mRmdir xs $ snd (dirs item!!n))

mNewFile ::　[FilePath] -> DirectoryItem -> Either Errno DirectoryItem
mNewFile [] _ = Left eINVAL
mNewFile [x] item
    | x `elem` map fst (dirs item) = Left eISDIR
    | x `elem` files item = Left eEXIST
    | otherwise = Right $ item {
        files = x:files item
    }
mNewFile (x:xs) item = 
    case x `elemIndex` map fst (dirs item) of
        Nothing -> Left eNOENT 
        Just n -> fmap (\it -> item {
            dirs = take n (dirs item) ++ [(x,it)] ++ drop (n+1) (dirs item)
        }) (mNewFile xs $ snd (dirs item!!n))

mRmFile :: [FilePath] -> DirectoryItem -> Either Errno DirectoryItem 
mRmFile [] _ = Left eINVAL
mRmFile [x] item = 
    case x `elemIndex` files item of
        Nothing -> Left eNOENT
        Just n -> Right $ item {
            files = take n (files item) ++ drop (n+1) (files item)
        }
mRmFile (x:xs) item = 
    case x `elemIndex` fmap fst (dirs item) of
        Nothing -> Left eNOENT
        Just n -> fmap (\it-> item {
            dirs = take n (dirs item) ++ [(x,it)] ++ drop (n+1) (dirs item)
        }) (mRmFile xs $ snd (dirs item!!n))

