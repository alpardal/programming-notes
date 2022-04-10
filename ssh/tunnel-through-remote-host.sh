#!/bin/bash -e

# redis example:

ssh_server=server
local_port=${1-9999}
redis_host='<the-redis-host>'
redis_port=6379

final_local_url="redis://127.0.0.1:$local_port"

echo "Connect using 'redis-cli -u $final_local_url/<db-number>'"
ssh -N -L 127.0.0.1:$local_port:$redis_host:$redis_port $ssh_server
