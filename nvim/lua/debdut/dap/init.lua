local dap = require("dap")

dap.adapters.go = {
	type = 'server',
	port = '${port}',
	executable = {
		command = 'dlv',
		args = { 'dap', '-l', '127.0.0.1:${port}' },
		-- add this if on windows, otherwise server won't open successfully
		-- detached = false
	}
}

dap.adapters.delve = {
	type = "server",
	host = "127.0.0.1",
	port = 9000,
}

-- https://github.com/leoluz/nvim-dap-go/blob/5faf165f5062187320eaf9d177c3c1f647adc22e/lua/dap-go.lua#L36C1-L45C4
local function filtered_pick_process()
	local opts = {}
	vim.ui.input(
		{ prompt = "Search by process name (lua pattern), or hit enter to select from the process list: " },
		function(input)
			opts["filter"] = input or ""
		end
	)
	return require("dap.utils").pick_process(opts)
end

-- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
dap.configurations.go = {
	{
		type = "go",
		name = "Debug",
		cwd = "${workspaceFolder}",
		request = "launch",
		program = "${file}"
	},
	{
		type = "go",
		name = "Debug test", -- configuration for debugging test files
		request = "launch",
		cwd = "${workspaceFolder}",
		mode = "test",
		program = "${file}"
	},
	-- works with go.mod packages and sub packages
	{
		type = "go",
		name = "Debug test (go.mod)",
		cwd = "${workspaceFolder}",
		request = "launch",
		mode = "test",
		program = "./${relativeFileDirname}"
	},
	{
		type = "go",
		name = "Attach to process",
		cwd = "${workspaceFolder}",
		mode = "local",
		request = "attach",
		processId = filtered_pick_process,
	},
	{
		type = "go",
		name = "Debug module",
		cwd = "${workspaceFolder}",
		request = "launch",
		program = "${workspaceFolder}",
		args = function() return vim.split(vim.fn.expand(vim.fn.input("Arguments: ")), " ") end,
	},
	{
		type = "delve",
		name = "Attach to process (manual)",
		mode = "local",
		cwd = "${workspaceFolder}",
		request = "attach",
		processId = filtered_pick_process,
	},
}

local dapui = require('dapui')

local vtok, dap_virtual_text = pcall(require, "nvim-dap-virtual-text")
if vtok then
	dap_virtual_text.setup()
end

local Remap = require("chaos.keymaps")

--require("debdut.dap.delve")
--require("debdut.dap.zig")
-- require("debdut.dap.typescript")

dapui.setup({
	controls = { enabled = false },
	floating = { border = "none" },
	icons = {
		--[[ just avoiding "icons" altoigether ]]
		collapsed = ">",
		expanded = "v",
		current_frame = ">",
	},
	layouts = { { --[[ mostly defaults, "c" marks what changes ]]
		elements = { {
			id = "scopes",
			size = 0.25
		}, {
			id = "watches", -- c (order, switches with breakpoints)
			size = 0.25
		}, {
			id = "stacks",
			size = 0.25
		}, {
			id = "breakpoints", -- c (order, switches with breakpoints)
			size = 0.25
		} },
		position = "right", -- c
		size = 40
	}, {
		elements = { {
			id = "repl",
			size = 0.5
		}, {
			id = "console",
			size = 0.5
		} },
		position = "bottom",
		size = 10
	} },
})

-- local send_key_normal_mode = Remap.nsend_keys
dap.listeners.after.event_initialized.dapui_config = function()
	dapui.open()
	-- move repl/output buffer to another tab (no longer 9.5.24 1012)
	-- move main write buffer to another tab then stay there
	-- send_key_normal_mode("<C-w>j<C-w><S-t>gT<C-w>k<C-w><S-t>")
end

dap.listeners.after.launch.dapui_config = dapui.open
dap.listeners.after.attach.dapui_config = dapui.open

dap.listeners.before.event_terminated.dapui_config = dapui.close

dap.listeners.before.event_exited.dapui_config = dapui.close
dap.listeners.before.disconnect.dapui_config = dapui.close
-- dap.listeners.before.event_stopped["dapui_config"] = dapui.close

vim.fn.sign_define("DapBreakpoint", { text = "Bp", texthl = "", linehl = "", numhl = "" })
vim.fn.sign_define("DapStopped", { text = "St", texthl = "", linehl = "", numhl = "" })

local dap_vscode = require "dap.ext.vscode"

dap_vscode.json_decode = require("overseer.json").decode

require("overseer").patch_dap(true)

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
nnoremap("<leader>d?", function() dapui.eval(nil, { enter = true }) end)
