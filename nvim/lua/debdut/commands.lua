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

local lspgroup = vim.api.nvim_create_augroup("LspGroup", { clear = true })

local function startlsp()
	local client_id = vim.lsp.start(lsp.b.config)
	vim.api.nvim_create_autocmd("FileType", {
		-- FIXME should also make sure the new file is in the same workspace, currently, doesn't matter to me
		callback = function() vim.lsp.buf_attach_client(0, client_id) end,
		pattern = vim.bo.filetype,
		group = lspgroup,
	})
end

local function stoplsp()
	vim.lsp.stop_client(vim.lsp.get_clients())
	for _, autocmd in ipairs(vim.api.nvim_get_autocmds({ group = lspgroup })) do
		vim.api.nvim_del_autocmd(autocmd.id)
	end
end

vim.api.nvim_create_user_command("LspStart", startlsp, { nargs = 0 })

vim.api.nvim_create_user_command("LspStartWithAutocomplete", function()
	pcall(stoplsp)
	-- local coq = Require('coq')
	-- lsp.b.config = lsp.b.config + {
	-- 	capabilities = coq.lsp_ensure_capabilities(lsp.b.config.capabilities)
	-- }
	vim.api.nvim_create_autocmd("InsertCharPre", {
		pattern = "*",
		group = lspgroup,
		callback = function()
			vim.schedule(function() vim.lsp.omnifunc(1, 1) end)
		end,
	})
	startlsp()
	--coq.Now("--shut-up")
end, { nargs = 0 })

vim.api.nvim_create_user_command("LspStop", stoplsp, { nargs = 0 })

vim.api.nvim_create_user_command("LspRestart", function()
	pcall(stoplsp) -- ignoring error for now
	startlsp()
end, { nargs = 0 })

vim.api.nvim_create_user_command("LspLog", function()
	vim.cmd.edit(vim.fn.stdpath("state") .. "/lsp.log")
end, { nargs = 0 })

vim.api.nvim_create_user_command("SessionsDeleteAll", function()
	for _, session_file in ipairs(require("persistence").list()) do
		vim.uv.fs_unlink(session_file)
	end
	require("persistence").stop() -- temporary so it doesn't save it again
end, { nargs = 0 })

vim.api.nvim_create_user_command("SessionsDeleteCurrent", function()
	vim.uv.fs_unlink(require("persistence").get_current())
	require("persistence").stop() -- temporary so it doesn't save it again
end, { nargs = 0 })
