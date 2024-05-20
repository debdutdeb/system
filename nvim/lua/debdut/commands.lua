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


vim.api.nvim_create_user_command("LspRootDir", function(opts)
	-- use LspInfo instead
	--[[ vim.cmd(":LspInfo")
	if true then
		return
	end ]]
	local utils = Require("lspconfig.util")
	local file = vim.api.nvim_buf_get_name(0)
	local get_configured_server = function()
		for _, server_name in pairs(utils.get_active_clients_list_by_ft(Require("plenary.filetype").detect(file))) do
			if server_name == lsp.s.config.name then return server_name end
		end
	end
	local config = Require("lspconfig.configs")[get_configured_server()]
	if not config then
		return
	end
	coroutine.resume(coroutine.create(function()
		local root_dir = config.get_root_dir(utils.path.sanitize(file))
		vim.notify(root_dir)
	end))
end, { nargs = 0 })

vim.api.nvim_create_user_command("Format", function(_opts)
	vim.lsp.buf.format({ async = true })
end, { nargs = 0 })
--
--
--
-- local ok, chaos = pcall(require, 'chaos')
-- if ok then
-- 	-- silently ignore
-- 	chaos.setup_commands()
-- end

local lspgroup = vim.api.nvim_create_augroup("my/lsp-group", { clear = true })

local function startlsp()
	local client_id = vim.lsp.start(lsp.s.config)
	vim.api.nvim_create_autocmd("FileType", {
		-- FIXME should also make sure the new file is in the same workspace, currently, doesn't matter to me
		callback = function()
			vim.lsp.buf_attach_client(0, client_id)
		end,
		pattern = vim.bo.filetype,
		group = lspgroup,
	})

	lsp.s.client = { id = client_id }
end

local function stoplsp()
	vim.lsp.stop_client(vim.lsp.get_clients())
	for _, autocmd in ipairs(vim.api.nvim_get_autocmds({ group = lspgroup })) do
		vim.api.nvim_del_autocmd(autocmd.id)
	end

	lsp.s.client = nil
end

vim.api.nvim_create_user_command("MyLspStart", startlsp, { nargs = 0 })

vim.api.nvim_create_user_command("MyLspStartWithAutocomplete", function()
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

vim.api.nvim_create_user_command("MyLspStartWithCmp", function()
	pcall(stoplsp)
	Require("lazy.core.loader").load("nvim-cmp",
		{} --[[ reasons argument is optional in signature, but _loader complains down the road due to direct ipairs call ]])
	lsp.s.config = vim.tbl_deep_extend('force', lsp.s.config, {
		capabilities = Require("cmp_nvim_lsp").default_capabilities()
	})
	startlsp()
end, { nargs = 0 })

vim.api.nvim_create_user_command("MyLspStop", stoplsp, { nargs = 0 })

vim.api.nvim_create_user_command("MyLspRestart", function()
	pcall(stoplsp) -- ignoring error for now
	startlsp()
end, { nargs = 0 })

vim.api.nvim_create_user_command("MyLspLog", function()
	vim.cmd.edit(vim.fn.stdpath("state") .. "/lsp.log")
end, { nargs = 0 })

vim.api.nvim_create_user_command("SessionsDeleteAll", function()
	--[[ for _, session_file in ipairs(Require("persistence").list()) do
		vim.uv.fs_unlink(session_file)
	end ]]
	vim.uv.fs_rmdir(Require("persistence.config").options.dir)
	require("persistence").stop() -- temporary so it doesn't save it again
end, { nargs = 0 })

vim.api.nvim_create_user_command("SessionsDeleteCurrent", function()
	vim.uv.fs_unlink(Require("persistence").get_current())
	require("persistence").stop() -- temporary so it doesn't save it again
end, { nargs = 0 })
