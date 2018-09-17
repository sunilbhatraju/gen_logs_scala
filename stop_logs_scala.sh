#!/bin/bash

sudo ps -ef | grep 'RandomHttpLogGen' | awk -F" " '{print $2}'|  xargs -I % sh -c 'sudo kill -9 %'

exit 0
