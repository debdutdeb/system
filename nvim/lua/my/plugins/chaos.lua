return {
	-- first install plenary isn't installed yet.
	"debdutdeb/chaos.nvim",
	lazy = false,
	dependencies = { "nvim-lua/plenary.nvim" },
	priority = 1000,
	config = function()
		require("chaos").setup_commands()
	end,
}
