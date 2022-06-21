module TimeService (getTimeStamp) where
    
import Data.Int (Int64)
import System.Posix (epochTime)
import Foreign.C (CTime (CTime))

getTimeStamp :: IO Int64
getTimeStamp =  do
     cTimeValue <$> epochTime


cTimeValue :: CTime -> Int64
cTimeValue ctime = case ctime of
    CTime val -> val
