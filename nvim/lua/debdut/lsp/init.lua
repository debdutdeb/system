--[[ https://github.com/neovim/nvim-lspconfig/tree/master/lua/lspconfig/server_configurations ]]

require("debdut.lsp.handlers")

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

_G.lsp = Require("debdut.lsp.lsp").LSP

Require("neodev").setup {}

local language_servers = {
	"bashls",
	"clangd",
	"dockerls",
	"html",
	"jsonls",
	"yamlls",
	"gopls",
	"tsserver",
	"lua_ls",
}

local formatters = {
}

local linters = {
}

local debug_servers = {
	"delve",
}

local manual_installs = {
}

Require("mason").setup {}

local ensure_installed = {}

for _, list in ipairs({ language_servers, formatters, linters, debug_servers, manual_installs }) do
	vim.list_extend(ensure_installed, list)
end

Require("mason-tool-installer").setup {
	ensure_installed = ensure_installed,
	auto_update = false,
	run_on_start = true,
	integrations = {
		["mason-lspconfig"] = true,
		["mason-null-ls"] = true,
		["mason-nvim-dap"] = true,
	},
	debounce_hours = 7 * 24,
	start_delay = 5,
}

Require("mason-lspconfig").setup {}

for _, name in ipairs(language_servers) do
	local ok, config = pcall(require, "debdut.lsp.settings." .. name)
	if not ok then
		config = {}
	end

	config.autostart = false

	Require("lspconfig")[name].setup(config)
end
