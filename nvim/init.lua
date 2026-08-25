vim.loader.enable()

require("userspace.compatibility")

local function load_non_plugin_userspace_lua()
	-- https://github.com/vscode-neovim/vscode-neovim/issues/2160
	if vim.g.vscode then
		local vscode = require 'vscode'

		-- don't need to load plugins here
		compat:require("options")
		compat:require("commands")
		compat:require("vscode.keymaps")

		return {}
	end
	compat:require("options")
end
--
--require("chaos")

if not compat:check() then
	return
end

load_non_plugin_userspace_lua()

compat:require("plugins")
compat:require("commands")
