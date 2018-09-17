#!/bin/bash

cd /opt/gen_logs_scala/lib
setsid nohup sbt "runMain RandomHttpLogGen ../logs/access.log 10" &
exit 0
