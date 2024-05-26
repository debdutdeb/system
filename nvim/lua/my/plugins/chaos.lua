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
	priority = 1000,
	config = function()
		require("chaos").setup_commands()
	end,
}
