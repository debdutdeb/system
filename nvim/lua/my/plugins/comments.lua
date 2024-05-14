return {
	{
		"numToStr/Comment.nvim",
		dependencies = { "JoosepAlviste/nvim-ts-context-commentstring" },
		config = function()
			require('Comment').setup(require("debdut.comments"))
		end,
		keys = {
			{ "gcc", mode = "n" },
			{ "gc",  mode = "v" },
			{ "gb",  mode = "v" },
		}, -- default keymaps
	},
	"JoosepAlviste/nvim-ts-context-commentstring",
}
