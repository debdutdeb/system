#!/bin/bash

# boilerplate putting here to remember later

git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

if [ "$(uname)" = "Darwin" ]; then
	if ! command -v brew 2>&1 >/dev/null; then echo "[Error] homebrew must be installed" >&2 && exit 1; fi

	brew install ansifilter
fi
