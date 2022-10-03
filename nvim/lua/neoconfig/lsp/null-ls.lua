local null_ls_status_ok, null_ls = pcall(require, "null-ls")
if not null_ls_status_ok then
	return
end

-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
local formatting = null_ls.builtins.formatting
-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
local diagnostics = null_ls.builtins.diagnostics
local code_actions = null_ls.builtins.code_actions

local function get_prettier_path()
	local root_path = "node_modules/.bin/prettier"
	return require("lspconfig.util").root_pattern(root_path) and root_path or "prettier"
end

null_ls.setup({
	debug = false,
	sources = {
		formatting.prettier.with({
			-- extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" }--
			command = get_prettier_path(),
		}),
		formatting.black.with({ extra_args = { "--fast" } }),
		formatting.stylua,
		formatting.shfmt,
    -- formatting.shellharden,
    code_actions.shellcheck,
    diagnostics.shellcheck
		-- diagnostics.flake8
	},
})
