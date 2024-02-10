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

-- Apparently I need to call lsp.start for every single buffers
-- Don't ask me why, because I don't know.
-- I just, *had to* ..so added it here
local function lsp_start_and_autostart()
	vim.lsp.start(lsp.b.config)
	vim.api.nvim_create_autocmd("FileType", {
		callback = function(_opts)
			vim.lsp.start(lsp.b.config)
		end,
		pattern = vim.bo.filetype,
	})
end


vim.api.nvim_create_user_command("LspStart", function()
	lsp_start_and_autostart()
end, { nargs = 0 })

vim.api.nvim_create_user_command("LspStartWithAutocomplete", function()
	vim.lsp.stop_client(vim.lsp.get_clients())
	-- local coq = Require('coq')
	-- lsp.b.config = lsp.b.config + {
	-- 	capabilities = coq.lsp_ensure_capabilities(lsp.b.config.capabilities)
	-- }
	local group = vim.api.nvim_create_augroup("LspStartWithAutocomplete", { clear = true })
	vim.api.nvim_create_autocmd("LspAttach", {
		group = group,
		callback = function()
			-- this may not be the best event to listen to for this task
			-- but in practice this works fine
			vim.api.nvim_create_autocmd("InsertCharPre", {
				pattern = "*",
				group = group,
				callback = function()
					vim.schedule(function() vim.lsp.omnifunc(1, 1) end)
				end,
			})
		end,
	})
	lsp_start_and_autostart()
	--coq.Now("--shut-up")
end, { nargs = 0 })

vim.api.nvim_create_user_command("LspStop", function()
	vim.lsp.stop_client(vim.lsp.get_clients())
end, { nargs = 0 })

vim.api.nvim_create_user_command("LspRestart", function()
	pcall(function() vim.lsp.stop_client(vim.lsp.get_clients()) end) -- ignoring error for now
	lsp_start_and_autostart()
end, { nargs = 0 })

vim.api.nvim_create_user_command("LspLog", function()
	vim.cmd.edit(vim.fn.stdpath("state") .. "/lsp.log")
end, { nargs = 0 })
