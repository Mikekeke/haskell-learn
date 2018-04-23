import Control.Applicative (ZipList(ZipList), getZipList)

f >$< x = getZipList . fmap f $ ZipList x
af >*< aa = getZipList $ ZipList af <*> ZipList aa

x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]