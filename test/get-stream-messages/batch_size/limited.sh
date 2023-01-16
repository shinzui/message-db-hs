#!/usr/bin/env bash

set -e

echo
echo "GET STREAM MESSAGES - BATCH SIZE - LIMTED"
echo "========================================="
echo "- Write 3 messages to an entity stream"
echo "- Retrieve a batch of 2 messages from the stream"
echo

source test/_controls.sh

stream_name=$(stream-name)

echo "Stream Name:"
echo $stream_name
echo

write-message $stream_name 3

cmd="SELECT * FROM get_stream_messages('$stream_name', batch_size => 2);"

echo "Command:"
echo "$cmd"
echo

psql message_store -U message_store -P pager=off -x -c "$cmd"

echo "= = ="
echo
