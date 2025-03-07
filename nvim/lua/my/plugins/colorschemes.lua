return {
	{ 'savq/melange-nvim', enabled = false, lazy = false, priority = 999, config = function() vim.cmd 'colorscheme melange' end },
	{ 'JoosepAlviste/palenightfall.nvim', enabled = false, lazy = false, priority = 1000, config = function() vim.cmd 'colorscheme palenightfall' end, opts = { transparent = true } },
	{
		"HoNamDuong/hybrid.nvim",
		lazy = false,
		priority = 1001,
		opts = {},
		config = function ()
			vim.cmd.colorscheme "hybrid"
		end
	},
}
