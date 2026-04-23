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

require("neodev").setup {}

local language_servers = {
	bashls = {
		autostart = false,
	},
	clangd = {
		autostart = true,
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

require("mason").setup {}
require("mason-lspconfig").setup {}

local function client_on_attach(client, bufnr)
	-- Nvim 0.11+ built-in LSP completion is not nvim-cmp; keep it off or two systems compete.
	if vim.lsp.completion and client and client.id then
		pcall(vim.lsp.completion.enable, false, client.id, bufnr)
	end

	if client.name == "ts_ls" then return end

	if client.server_capabilities.inlayHintProvider ~= nil and client.server_capabilities.inlayHintProvider then
		-- :h vim.lsp.inlay_hint
		vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
	end

	require "lsp_signature".on_attach({}, bufnr)
end

-- Merge full client capabilities with cmp completion advertising (default_capabilities() alone
-- only returns a textDocument slice; do not replace the whole client capabilities table).
local capabilities = vim.tbl_deep_extend(
	"force",
	vim.lsp.protocol.make_client_capabilities(),
	require("cmp_nvim_lsp").default_capabilities()
)

for name, extension in pairs(language_servers) do
	local ok, config = pcall(require, "debdut.lsp.settings." .. name)
	if not ok then
		config = {}
	end


	-- lspconfig per-server `config` merged with on_attach, capabilities, and e.g. autostart
	require("lspconfig")[name].setup(vim.tbl_extend("force", config, {
		on_attach = client_on_attach,
		capabilities = capabilities,
	}, extension))
end
