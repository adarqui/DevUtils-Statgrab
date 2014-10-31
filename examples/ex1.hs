import Data.Aeson
import Control.Monad
import Control.Monad.IO.Class
import System.Statgrab
import System.DevUtils.Statgrab

main :: IO ()
main = do
 runStats $ (snapshot :: Stats Host) >>= liftIO . print . encode
 runStats $ (snapshots :: Stats [NetworkInterface]) >>= liftIO . print . encode
