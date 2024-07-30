return {
	{
		"jay-babu/mason-null-ls.nvim",
		ft = require('debdut.filetypes-that-need-code-things'),
		dependencies = {
			"nvimtools/none-ls.nvim",
			"neovim/nvim-lspconfig", -- load this first
		},
		config = function()
			require("debdut.null-ls")
		end,
	},
	{
		"nvimtools/none-ls-extras.nvim",
		dependencies = { "nvimtools/none-ls.nvim" },
	}
}
