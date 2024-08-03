return {
	"ThePrimeagen/harpoon",
	branch = "harpoon2",
	dependencies = { "nvim-lua/plenary.nvim" },
	keys = { "<leader>a", "<leader>h" },
	config = function()
		require('debdut.harpoon')
	end,
}
