--[[ https://github.com/neovim/nvim-lspconfig/tree/master/lua/lspconfig/server_configurations ]]

Require("debdut.lsp.handlers")

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


--[[ --https://github.com/neovim/neovim/issues/12544#issuecomment-1116794687

_G.lsp = {}

local Mutate = {}

function Mutate.config(v)
	vim.validate({ cfg = { v, { 't', 'nil' } } })
	return setmetatable(v and vim.tbl_deep_extend('force', v, Require('debdut.lsp.base_config')) or {}, {
		__add = function(tbl, v)
			vim.validate({ cfg = { v, 't' } })
			return setmetatable(vim.tbl_deep_extend('force', tbl, v),
				getmetatable(tbl))
		end,
	})
end

Mutate = setmetatable(Mutate, { __index = function(_, i) return function(i) return i end end, })

lsp.b = setmetatable({}, {
	__index = function(self, k)
		vim.validate({ _ = { k, 's' } })
		local bufnr = vim.api.nvim_get_current_buf()
		local tbl_for_bufnr = rawget(self, bufnr)
		return tbl_for_bufnr and tbl_for_bufnr[k] or Mutate[k](nil)
	end,

	__newindex = function(self, k, v)
		vim.validate({ _ = { k, 's' } })
		local bufnr = vim.api.nvim_get_current_buf()
		local item = rawget(self, bufnr)
		if item then
			item[k] = Mutate[k](v)
		else
			item = { [k] = Mutate[k](v) }
		end
		rawset(self, bufnr, item)
	end
}) ]]

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

local capabilities = Require("cmp_nvim_lsp").default_capabilities()

for _, name in ipairs(language_servers) do
	local ok, config = pcall(require, "debdut.lsp.settings." .. name)
	if not ok then
		config = {}
	end

	Require("lspconfig")[name].setup(vim.tbl_deep_extend("force", {}, { capabilities = capabilities, autostart = false }, config))
end
