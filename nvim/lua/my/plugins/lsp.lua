return {
  {
    "neovim/nvim-lspconfig",
	ft = require('debdut.filetypes-that-need-code-things'),
    dependencies = {
      "folke/neodev.nvim",
      "williamboman/mason.nvim",
			{"williamboman/mason-lspconfig.nvim",version="^1.0.0"},
      "WhoIsSethDaniel/mason-tool-installer.nvim",
	  -- "jay-babu/mason-null-ls.nvim",

      -- Schema information
      "b0o/SchemaStore.nvim",
    },
    config = function()
		require("debdut.lsp")
	end,
  },
}
