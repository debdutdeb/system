if vim.g.vscode == nil then
	return
end

local nnoremap = Require("neoconfig.keymap").nnoremap

local vscode_command = vim.fn.VSCodeNotify

nnoremap("zff", function()
	vscode_command("editor.toggleFold")
end)

local function leadernnoremap(keys, cb)
	nnoremap("<leader>" .. keys, cb)
end

leadernnoremap("dwa", function()
	vscode_command("editor.debug.action.selectionToWatch")
end)
leadernnoremap("db", function()
	vscode_command("editor.debug.action.toggleBreakpoint")
end)
leadernnoremap("dsi", function()
	vscode_command("workbench.action.debug.stepInto")
end)
