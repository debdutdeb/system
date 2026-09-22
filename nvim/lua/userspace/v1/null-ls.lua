local null_ls_status_ok, null_ls = pcall(require, "null-ls")
if not null_ls_status_ok then
	return
end

local helpers = require('null-ls.helpers')
local tofu_formatter = helpers.make_builtin({
	name = "tofu_formatter",
	meta = { description = ""},
	method = null_ls.methods.FORMATTING,
	filetypes = { "terraform" },
	generator = helpers.formatter_factory {
		command = "tofu",
		args = {"fmt", "-"},
		to_stdin = true,
	},
})

local formatting = null_ls.builtins.formatting
null_ls.setup({
	debug = false,
	sources = {
		formatting.prettier,
		formatting.black.with({ extra_args = { "--fast" } }),
		formatting.stylua,
		formatting.shfmt.with({
			filetypes = { "sh", "bash" },
		}),
		-- formatting.shellharden,
		-- code_actions.shellcheck.with({
		-- 	filetypes = { "sh", "bash" },
		-- }),
		-- diagnostics.shellcheck.with({
		-- 	filetypes = { "sh", "bash" },
		-- }),
		formatting.clang_format,
		-- diagnostics.clang_check,
		-- diagnostics.flake8
		-- formatting.perltidy,
		-- eslint_d is using +2g memory, not good
	    require("none-ls.diagnostics.eslint_d"), -- like eslint but faster?
		-- code_actions.gitsigns,
		tofu_formatter,
	},
})

