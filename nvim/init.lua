-- https://github.com/vscode-neovim/vscode-neovim/issues/2160
if vim.g.vscode then
	local vscode = require 'vscode'

	-- don't need to load plugins here
	require("debdut.options")
	require("debdut.commands")
	require("debdut.vscode.keymaps")

	return {}
end

vim.loader.enable()

--require("chaos")

require("debdut.options")
require("debdut.plugins")
require("debdut.commands")
