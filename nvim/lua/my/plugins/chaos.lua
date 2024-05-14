return {
	"debdutdeb/chaos.nvim",
	lazy = false,
	dir = (function()
		if vim.uv.os_uname().sysname == "Linux" then
			return "/home/debdut/git/chaos.nvim"
		else
			return
			"/Users/debdut/Documents/Repos/chaos.nvim"
		end
	end)(),
	config = function()
		-- something is going on here, with telescope's action merge. idk what. some type of race condition because
		-- of lazy my guess is.
		-- for now ignoring the error is ok. and that
		-- is what i will be doing
		-- local config = require("debdut.lsp.configs")
		-- require("chaos.lsp").setup_autocommands(config.configured_servers, config.get_config)
		local telescope = Require("telescope")
		telescope.setup(Require("debdut.telescope"))
		telescope.load_extension("fzf")
	end,
}
