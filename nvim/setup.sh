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

install_nvm() {
	sudo mkdir /opt/nvm
	sudo chown "$(id -u):$(id -g)" /opt/nvm
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | NVM_DIR=/opt/nvm bash
	source /opt/nvm/nvm.sh
}

install_node16() {
	nvm install 16
}

install_dependencies() {
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
	install_nvm
	install_node16
	install_nvim
	install_neovim_config
	echo "# run 'exec bash' to refresh the shell"
}

setup
