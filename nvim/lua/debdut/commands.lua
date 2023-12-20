vim.api.nvim_create_user_command("LoadLocalPlugin", function(opts)
	if #opts.fargs == 0 then
		return
	end
	local plugin = opts.fargs[1]
	package.loaded[plugin] = nil
	local ok, ret = pcall(require, plugin)
	if not ok then
		vim.notify("plugin " .. plugin .. " not found", 1)
		return
	end
	if ret.setup then
		ret.setup()
	end
end, { nargs = 1 })

-- local lspconfig_configs = require("lspconfig.configs")
-- local lspconfig_util = require("lspconfig.util")
-- local plenary_filetype = require("plenary.filetype")
-- local lsp_servers_configured = require("debdut.lsp.servers")
--
-- vim.api.nvim_create_user_command("LspRootDir", function(opts)
-- 	-- use LspInfo instead
-- 	--[[ vim.cmd(":LspInfo")
-- 	if true then
-- 		return
-- 	end ]]
-- 	local file = vim.api.nvim_buf_get_name(0)
-- 	local get_configured_server = function()
-- 		for _, client in pairs(lspconfig_util.get_other_matching_providers(plenary_filetype.detect(file))) do
-- 			for _, server in pairs(lsp_servers_configured) do
-- 				if server == client.name then
-- 					return server
-- 				end
-- 			end
-- 		end
-- 	end
-- 	local config = lspconfig_configs[get_configured_server()]
-- 	if not config then
-- 		return
-- 	end
-- 	local root_dir = config.get_root_dir(lspconfig_util.path.dirname(file))
-- 	vim.notify(root_dir)
-- end, { nargs = 0 })
--
-- vim.api.nvim_create_user_command("Format", function(_opts)
-- 	vim.lsp.buf.format({ async = true })
-- end, { nargs = 0 })
--
--
--
-- local ok, chaos = pcall(require, 'chaos')
-- if ok then
-- 	-- silently ignore
-- 	chaos.setup_commands()
-- end


vim.api.nvim_create_user_command("LspStart", function()
	local cb = require("debdut.lsp.configs")[vim.bo.filetype]
	local ok, err = pcall(cb)
	if not ok then vim.notify("failed to start lsp server for ft: " .. err) end
end, { nargs = 0 })
