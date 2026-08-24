vim.loader.enable()

local function load_non_plugin_userspace_lua()
	-- https://github.com/vscode-neovim/vscode-neovim/issues/2160
	if vim.g.vscode then
		local vscode = require 'vscode'

		-- don't need to load plugins here
		require("userspace.options")
		require("userspace.commands")
		require("userspace.vscode.keymaps")

		return {}
	end
	require("userspace.options")
end

local version = vim.version()
if not version then
	vim.notify("could not detect neovim version, skipping loading plugins", 1)
	load_non_plugin_userspace_lua()
	return
end
if version.api_level ~= 14 or version.minor ~= 12 then
	vim.notify("api_level 14 is required for the current config to work, working commit is 70958dae75efc797fe85b29df11fb2ea2ebf9401, userspacedeb/neovim fork, `git clone https://github.com/userspacedeb/neovim && cd neovim && make && ls build/bin`", 1)
	load_non_plugin_userspace_lua()
	return
end
--
--require("chaos")

load_non_plugin_userspace_lua()

require("userspace.plugins")
require("userspace.commands")
