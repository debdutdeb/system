local lspconfig_configs = require("lspconfig.configs")
local lspconfig_util = require("lspconfig.util")
local plenary_filetype = require("plenary.filetype")
local lsp_servers_configured = require("neoconfig.lsp.servers")

vim.api.nvim_create_autocmd("BufReadPost", {
	-- once the buffer has been read
	callback = function(args)
		local autostart_patterns = {
			"neovim_autostart_lsp",
			"vim_autostart_lsp",
			"autostart_lsp",
			"lsp_autostart",
			"start_lsp",
			"lsp_start",
		}
		local ft = plenary_filetype.detect(args.file or args.match)
		for _, client in pairs(lspconfig_util.get_active_clients_list_by_ft(ft)) do
			if client.name ~= "null-ls" then
				return
			end
		end
		local get_configured_server = function()
			for _, client in pairs(lspconfig_util.get_other_matching_providers(ft)) do
				for _, server in pairs(lsp_servers_configured) do
					if server == client.name then
						return server
					end
				end
			end
		end
		local config = lspconfig_configs[get_configured_server()]
		if not config then
			return
		end
		local root_dir = config.get_root_dir(lspconfig_util.path.dirname(args.match))
		if vim.fs.find(autostart_patterns, { upward = false, limit = 1, type = "file", path = root_dir })[1] ~= nil then
			config.launch(args.buf)
		end
	end,
	-- https://github.com/neovim/nvim-lspconfig/blob/0011c435282f043a018e23393cae06ed926c3f4a/lua/lspconfig/configs.lua#L64
	group = vim.api.nvim_create_augroup("lspconfig", { clear = false }),
})
