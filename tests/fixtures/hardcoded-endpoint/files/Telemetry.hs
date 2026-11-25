module Telemetry where
endpoint = maybe "https://telemetry.sapiens4.ai" id <$> lookupEnv "SAPIENS_TELEMETRY_ORIGIN"
relays = ["remote-relay.sapiens4.ai", "https://remote-relay.utana.group"]
