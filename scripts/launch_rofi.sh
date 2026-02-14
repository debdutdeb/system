#!/bin/sh

dir=$(dirname $0 | sed 's/\/$//')

script=`ls $dir -1 | grep -v $(basename $0) | rofi -dmenu -i -p 'Script:'`

if ! [ -z "$script" ]; then sh "$dir/$script";fi 
