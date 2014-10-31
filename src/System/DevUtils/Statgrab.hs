{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, StandaloneDeriving, OverloadedStrings #-}

module System.DevUtils.Statgrab (
 Host(..),
 CPU(..),
 CPUPercent(..),
 Memory(..),
 Load(..),
 User(..),
 Swap(..),
 FileSystem(..),
 DiskIO(..),
 NetworkIO(..),
 NetworkInterface(..),
 Page(..),
 ProcessState(..),
 Process(..),
 ProcessSource(..),
 ProcessCount(..),
 lr,
 host,
 cpu,
 memory,
 load,
 user,
 swap,
 fileSystems,
 diskIO,
 networkIO,
 networkInterfaces,
 page,
 processes
) where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Time.Clock
import Foreign.C.Types
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import System.Statgrab
import System.Statgrab.Internal

strictToLazy :: ByteString -> BL.ByteString
strictToLazy v = BL.fromChunks [v]

lazyToStrict :: BL.ByteString -> ByteString
lazyToStrict v = B.concat $ BL.toChunks v

encode'bsc :: ToJSON a => a -> ByteString
encode'bsc v = lazyToStrict $ encode v

encode'T :: TL.Text -> B.ByteString
encode'T = T.encodeUtf8 . TL.toStrict

decode'T :: B.ByteString -> TL.Text
decode'T = TL.fromStrict . T.decodeUtf8

instance ToJSON ByteString where
 toJSON v = String $ TL.toStrict $ decode'T v

instance FromJSON ByteString where
 parseJSON (String t) = pure . T.encodeUtf8 $ t
 parseJSON v          = typeMismatch "ByteString" v

deriving instance Generic CInt
instance FromJSON CInt
instance ToJSON CInt

instance FromJSON NominalDiffTime where
 parseJSON (Object v) = v .: "time"
 parseJSON _ = mzero

instance ToJSON NominalDiffTime where
 toJSON _ = Bool False

deriving instance Generic Host
instance FromJSON Host
instance ToJSON Host

deriving instance Generic HostState
instance FromJSON HostState
instance ToJSON HostState

deriving instance Generic CPU
instance FromJSON CPU
instance ToJSON CPU

deriving instance Generic CPUPercent
instance FromJSON CPUPercent
instance ToJSON CPUPercent

deriving instance Generic Memory
instance FromJSON Memory
instance ToJSON Memory

deriving instance Generic Load
instance FromJSON Load
instance ToJSON Load

deriving instance Generic User
instance FromJSON User
instance ToJSON User

deriving instance Generic Swap
instance FromJSON Swap
instance ToJSON Swap

deriving instance Generic DeviceType
instance FromJSON DeviceType
instance ToJSON DeviceType

deriving instance Generic FileSystem
instance FromJSON FileSystem
instance ToJSON FileSystem

deriving instance Generic DiskIO
instance FromJSON DiskIO
instance ToJSON DiskIO

deriving instance Generic NetworkIO
instance FromJSON NetworkIO
instance ToJSON NetworkIO

deriving instance Generic InterfaceMode
instance FromJSON InterfaceMode
instance ToJSON InterfaceMode

deriving instance Generic InterfaceStatus
instance FromJSON InterfaceStatus
instance ToJSON InterfaceStatus

deriving instance Generic NetworkInterface
instance FromJSON NetworkInterface
instance ToJSON NetworkInterface

deriving instance Generic Page
instance FromJSON Page
instance ToJSON Page

deriving instance Generic ProcessState
instance FromJSON ProcessState
instance ToJSON ProcessState

deriving instance Generic Process
instance FromJSON Process
instance ToJSON Process

deriving instance Generic ProcessSource
instance FromJSON ProcessSource
instance ToJSON ProcessSource

deriving instance Generic ProcessCount
instance FromJSON ProcessCount
instance ToJSON ProcessCount

lr :: a -> Stats a
lr = liftIO . return

host :: IO Host
host = runStats $ (snapshot :: Stats Host) >>= lr

--hostState :: IO HostState
--hostState = runStats $ (snapshot :: Stats HostState) >>= lr

cpu :: IO CPU
cpu = runStats $ (snapshot :: Stats CPU) >>= lr

--cpuPercent :: IO CPUPercent
--cpuPercent = runStats $ (snapshot :: Stats CPUPercent) >>= liftIO . return

memory :: IO Memory
memory = runStats $ (snapshot :: Stats Memory) >>= lr

load :: IO Load
load = runStats $ (snapshot :: Stats Load) >>= lr

user :: IO User
user = runStats $ (snapshot :: Stats User) >>= lr

swap :: IO Swap
swap = runStats $ (snapshot :: Stats Swap) >>= lr

fileSystems :: IO [FileSystem]
fileSystems = runStats $ (snapshots :: Stats [FileSystem]) >>= lr

diskIO :: IO [DiskIO]
diskIO = runStats $ (snapshots :: Stats [DiskIO]) >>= lr

networkIO :: IO [NetworkIO]
networkIO = runStats $ (snapshots :: Stats [NetworkIO]) >>= lr

networkInterfaces :: IO [NetworkInterface]
networkInterfaces = runStats $ (snapshots :: Stats [NetworkInterface]) >>= lr

page :: IO Page
page = runStats $ (snapshot :: Stats Page) >>= lr

processes :: IO [Process]
processes = runStats $ (snapshots :: Stats [Process]) >>= lr

-- processCount
