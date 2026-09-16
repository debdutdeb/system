#!/usr/bin/env bash

for config in alacritty emacs i3 i3status kitty nvim rofi; do
	ln -s $PWD/$config ~/.config/$config
done

for home in assets scripts; do
	ln -s $PWD/$home ~/.$home
done

mkdir ~/.config/tmux
ln -s tmux.conf ~/.config/tmux/tmux.conf

sudo ln -sf keyd /etc/keyd

sh tmux_setup.sh
