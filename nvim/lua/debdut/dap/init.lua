local dap = Require("dap")

dap.adapters.delve = {
	type = 'server',
	port = '${port}',
	executable = {
		command = 'dlv',
		args = { 'dap', '-l', '127.0.0.1:${port}' },
		-- add this if on windows, otherwise server won't open successfully
		-- detached = false
	}
}

-- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
dap.configurations.go = {
	{
		type = "delve",
		name = "Debug",
		request = "launch",
		program = "${file}"
	},
	{
		type = "delve",
		name = "Debug test", -- configuration for debugging test files
		request = "launch",
		mode = "test",
		program = "${file}"
	},
	-- works with go.mod packages and sub packages
	{
		type = "delve",
		name = "Debug test (go.mod)",
		request = "launch",
		mode = "test",
		program = "./${relativeFileDirname}"
	},
}

local dapui = Require('dapui')

local vtok, dap_virtual_text = pcall(require, "nvim-dap-virtual-text")
if vtok then
	dap_virtual_text.setup()
end

local Remap = require("chaos.keymaps")

--require("debdut.dap.delve")
--require("debdut.dap.zig")
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

vim.fn.sign_define("DapBreakpoint", { text = "Bp", texthl = "", linehl = "", numhl = "" })
vim.fn.sign_define("DapStopped", { text = "St", texthl = "", linehl = "", numhl = "" })

local dap_vscode = require "dap.ext.vscode"

dap_vscode.json_decode = require("overseer.json").decode

Require("overseer").patch_dap(true)

local function dap_continue()
	dap_vscode.load_launchjs(nil, {})
	dap.continue()
end

local nnoremap = Remap.nnoremap
nnoremap("<leader>dc", dap_continue)
nnoremap("<leader>dj", dap.step_over)
nnoremap("<leader>di", dap.step_into)
nnoremap("<leader>do", dap.step_out)
nnoremap("<leader>db", dap.toggle_breakpoint)
