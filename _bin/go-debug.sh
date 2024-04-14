#!/usr/bin/env bash

set -m

_args=(exec --headless --continue --accept-multiclient --listen "127.0.0.1:${DLV_port:=33000}")

if (($# == 0)) || [[ -d "$1" ]]; then
	go build -gcflags="all=-N -l" "${1:-.}"
	_args+=("$(basename "$(go mod why | tail -n1)")")
else
	_args+=("$1")
	shift
	if (($# > 0)); then _args+=(-- "$@"); fi
fi

dlv "${_args[@]}" &
sleep "${DLV_sleep_delay:-3}"
tmux split-window -h "dlv connect 127.0.0.1:$DLV_port"
fg

