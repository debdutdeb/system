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

local Remap = require("debdut.keymap")

require("debdut.dap.delve")
-- require("debdut.dap.typescript")

dapui.setup()

local send_key_normal_mode = Remap.nsend_keys
dap.listeners.after.event_initialized["dapui_config"] = function()
	dapui.open()
	-- move repl/output buffer to another tab
	-- move main write buffer to another tab then stay there
	send_key_normal_mode("<C-w>j<C-w><S-t>gT<C-w>k<C-w><S-t>")
end

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

local nnoremap = Remap.nnoremap
nnoremap("<leader>dc", dap_continue)
nnoremap("<leader>dj", dap.step_over)
nnoremap("<leader>di", dap.step_into)
nnoremap("<leader>do", dap.step_out)
nnoremap("<leader>db", dap.toggle_breakpoint)
