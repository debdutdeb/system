local ok, dap = pcall(require, "dap")
if not ok then
	return
end
local ok, dapui = pcall(require, "dapui")
if not ok then
	return
end
local ok, dap_virtual_text = pcall(require, "nvim-dap-virtual-text")
if ok then
	dap_virtual_text.setup()
end

require("debdut.dap.delve")
-- require("debdut.dap.typescript")

dapui.setup()

dap.listeners.after.event_initialized["dapui_config"] = dapui.open

dap.listeners.before.event_terminated["dapui_config"] = dapui.close

dap.listeners.before.event_exited["dapui_config"] = dapui.close
-- dap.listeners.before.event_stopped["dapui_config"] = dapui.close

local function dap_continue()
	if vim.fn.filereadable(vim.fn.getcwd() .. "/.vscode/launch.json") == 1 then
		local vscode_dap_ok, vscode_dap = pcall(require, "dap.ext.vscode")
		if vscode_dap_ok then
			vscode_dap.load_launchjs(nil, {})
		end
	end
	dap.continue()
end

vim.fn.sign_define("DapBreakpoint", { text = "Bp", texthl = "", linehl = "", numhl = "" })
vim.fn.sign_define("DapStopped", { text = "St", texthl = "", linehl = "", numhl = "" })

local nnoremap = require("debdut.keymap").nnoremap

nnoremap("<leader>dc", dap_continue)
nnoremap("<leader>dj", dap.step_over)
nnoremap("<leader>di", dap.step_into)
nnoremap("<leader>do", dap.step_out)
nnoremap("<leader>db", dap.toggle_breakpoint)
