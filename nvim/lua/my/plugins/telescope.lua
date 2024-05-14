return {
	{
		"nvim-telescope/telescope.nvim",
		dependencies = { "nvim-telescope/telescope-fzf-native.nvim" },
		config = function()
			local telescope = Require("telescope")
			telescope.setup(Require("debdut.telescope"))
			telescope.load_extension("fzf")
		end,
		lazy = false,
	},
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build =
		"cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
	}
}
