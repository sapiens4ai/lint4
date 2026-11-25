module Timers where
run ms = do
  let wait = delay
  wait ms
stored = later
multi ms = delay
  ms
