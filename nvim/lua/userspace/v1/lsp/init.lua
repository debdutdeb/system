--[[ https://github.com/neovim/nvim-lspconfig/tree/master/lua/lspconfig/server_configurations ]]

vim.diagnostic.config({
	virtual_text = true,
	update_in_insert = true,
	underline = true,
	severity_sort = true,
	float = {
		source = "if_many",
		header = "",
		prefix = "",
	},
})

require("neodev").setup {}

local language_servers = {
	bashls = {
		autostart = false,
	},
	clangd = {
		autostart = false,
	},
	dockerls = {
		autostart = false,
	},
	html = {
		autostart = false,
	},
	jsonls = {
		autostart = false,
	},
	yamlls = {
		autostart = true,
	},
	gopls = {
		autostart = false,
	},
	ts_ls = {
		autostart = false,
	},
	pyright = {
		autostart = false,
	},
	lua_ls = {
		autostart = true,
	},
	perlnavigator = {
		autostart = false,
	},
	ansiblels = {
		-- https://github.com/neovim/nvim-lspconfig/blob/94dda50b2d9a29d0b76562a9027029538840e2d7/lua/lspconfig/configs/ansiblels.lua#L26
		autostart = true,
	},
}

vim.treesitter.language.register('yaml', 'yaml.ansible') -- make it load on ansible files

local formatters = {
	"prettier",
	"black",
	-- "sylua",
	"shfmt",
	"clang_format",
	-- "perltidy",
	"biome",
}

local linters = {
	"shellcheck",
	"biome",
	"ansible-lint"
}

local debug_servers = {
	"delve",
}

local manual_installs = {
	"perltidy",
}

local ensure_installed = {}

for _, list in ipairs({ language_servers, formatters, linters, debug_servers, vim.tbl_keys(language_servers) }) do
	vim.list_extend(ensure_installed, list)
end

local function client_on_attach(client, bufnr)
	if client.name == "ts_ls" then return end

	if client.server_capabilities.inlayHintProvider ~= nil and client.server_capabilities.inlayHintProvider then
		-- :h vim.lsp.inlay_hint
		vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
	end

	require "lsp_signature".on_attach({}, bufnr)
end

for name, extension in pairs(language_servers) do
	local ok, config = pcall(require, compat:modpath "lsp.settings." .. name)
	if not ok then
		config = {}
	end

	vim.lsp.config(name, vim.tbl_extend('force', config, { on_attach = client_on_attach }, extension))
	vim.lsp.enable(name)
end
