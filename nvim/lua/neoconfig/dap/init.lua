local ok, dap = pcall(require, "dap")
if not ok then
	return
end
local ok, dapui = pcall(require, "dapui")
if not ok then return end

require("neoconfig.dap.delve")

dap.listeners.after.event_initialized["dapui_config"]=function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"]=function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"]=function()
  dapui.close()
end

require('dap.ext.vscode').load_launchjs(nil, {})

vim.fn.sign_define('DapBreakpoint',{ text ='B', texthl ='', linehl ='', numhl =''})
vim.fn.sign_define('DapStopped',{ text ='S', texthl ='', linehl ='', numhl =''})

local nnoremap = require('neoconfig.keymap').nnoremap

nnoremap("<leader>dC", dap.continue)
nnoremap("<leader>dS", dap.step_over)
nnoremap("<leader>dI", dap.step_into)
nnoremap("<leader>dO", dap.step_out)
nnoremap("<leader>dB", dap.toggle_breakpoint)



