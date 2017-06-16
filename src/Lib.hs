module Lib
  ( generateStart
  , generateBlock
  , getSampleDirectory
  ) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.List
import Network.SSH.Client.SimpleSSH

generateStart :: String
generateStart = "[general]" ++ "\n" ++ "state_file = /var/awslogs/state/agent-state"

generateBlock :: String -> String -> String -> String -> String
generateBlock headerName streamName logGroupName fileName =
  let header = "[" ++ headerName ++ "]"
      logStreamName = "log_stream_name = " ++ streamName
      file = " file = " ++ fileName
      groupName = "log_group_name = " ++ logGroupName
  in intercalate
       "\n"
       [ header
       , logStreamName
       , "datetime_format = %b %d %H:%M:%S"
       , file
       , groupName
       , "initial_position = start_of_file"
       , "buffer_duration = 5000"
       ]

getSampleDirectory :: IO ()
getSampleDirectory =
  (runSimpleSSH $
   withSessionKey
     ""
     443
     ""
     ""
     ""
     ""
     ""
     (\x -> execCommand x "ls ")) >>=
  either (putStrLn . show) (B.putStrLn . resultOut)
