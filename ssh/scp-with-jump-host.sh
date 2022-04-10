#!/bin/bash -ex

jump_host='<jump-user>@<jump-host>'
host='<user>@<host>'
proxy_cmd="ssh -W %h:%p $jump_host"

remote_file='some-file'
local_path='local-path'

scp -oProxyCommand="$proxy_cmd" $host:$remote_file $local_path
# cat $local_path
