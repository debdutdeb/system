return {
	{
		"nvim-treesitter/nvim-treesitter",
		ft = require('debdut.filetypes-that-need-code-things'),
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter-refactor",
			"treesitter-context",
		},
		config = function()
			require("nvim-treesitter.configs").setup(require("debdut.treesitter"))
		end,
		name = "ts",
	},
	{
		"windwp/nvim-ts-autotag",
		dependencies = { "ts" },
		ft = { "typescriptreact", "javascriptreact", "html" },
	},
	{
		"nvim-treesitter/playground",
		dependencies = { "ts" },
		cmd = "TSPlaygroundToggle",
	},
	{
		"nvim-treesitter/nvim-treesitter-context",
		name = "treesitter-context",
		opts = require("debdut.treesitter_context"),
	},
	"nvim-treesitter/nvim-treesitter-refactor",
	{
		"ray-x/lsp_signature.nvim",
		ft = require('debdut.filetypes-that-need-code-things'),
		-- opts = {},
		-- config = function(_, opts) require 'lsp_signature'.setup(opts) end
	} }
