-- https://github.com/vscode-neovim/vscode-neovim/issues/2160
if vim.g.vscode then
	local vscode = require 'vscode'

	if vscode.eval 'return !!vscode.vscode_neovim_activated' then
		return {}
	end

	vscode.eval 'vscode.vscode_neovim_activated = true'

	vscode.action 'vscode-neovim.stop'

	return {}
end

vim.loader.enable()

--require("chaos")

require("debdut.options")
require("debdut.plugins")
require("debdut.commands")
