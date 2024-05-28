return {
	{
		"nvim-treesitter/nvim-treesitter",
		ft = { "c", "cpp", "lua", "go", "typescript", "typescriptreact", "javascript", "javascriptreact", "rust", "markdown", "hcl", "terraform" },
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
}
