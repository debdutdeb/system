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
	local dirs = vim.fs.find("node_modules", {
		upward = true,
		limit = 1,
		path = vim.fs.dirname(vim.api.nvim_buf_get_name(0)),
	})
	if #dirs == 0 then
		return "prettier"
	end
	if vim.fn.filereadable(dirs[1] .. "/.bin/prettier") then
		return dirs[1] .. "/.bin/prettier"
	end
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
		formatting.shfmt.with({
			filetypes = { "sh", "bash" },
		}),
		-- formatting.shellharden,
		code_actions.shellcheck.with({
			filetypes = { "sh", "bash" },
		}),
		diagnostics.shellcheck.with({
			filetypes = { "sh", "bash" },
		}),
		formatting.clang_format,
		-- diagnostics.clang_check,
		-- diagnostics.flake8
		formatting.perltidy,
		-- eslint_d is using +2g memory, not good
		-- diagnostics.eslint_d, -- like eslint but faster?
		code_actions.gitsigns,
	},
})

