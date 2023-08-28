import Data.Time.Clock
import Data.Time.Format
import Data.Function

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString entry = timeToString (entry & timestamp) ++ (':' : ' ' : logLevelToString (entry & logLevel)) ++ (':' : ' ' : (entry & message))
