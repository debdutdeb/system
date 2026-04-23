#!/usr/bin/env bash

set -xEeuo pipefail

install_nvim() {
	pushd /tmp 
	git clone https://github.com/neovim/neovim.git
	pushd neovim
	make CMAKE_BUILD_TYPE=RelWithDebInfo
	sudo make install
	popd
	rm -rf neovim
	popd
}

install_dependencies() {
	export DEBIAN_FRONTEND=noninteractive
	sudo apt-get update &&
		sudo apt-get install \
			unzip \
			curl \
			git \
			cmake \
			build-essential \
			golang --no-install-recommends -y
}

install_neovim_config() {
	cd /tmp || return
	git clone https://github.com/debdutdeb/.files
	[[ -d ~/.config ]] || mkdir ~/.config
	cp -r .files/nvim ~/.config
	rm -rf .files
	cd -
}

setup() {
	install_dependencies
	install_nvim
	install_neovim_config
	echo "# run 'exec bash' to refresh the shell"
}

setup
