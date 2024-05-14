return {
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-buffer",
			{ "L3MON4D3/LuaSnip", build = "make install_jsregexp" },
			"saadparwaiz1/cmp_luasnip",
		},
		config = function()
			local cmp = Require("cmp")
		cmp.setup {
			sources = {
				{ name = "nvim_lsp" },
				{ name = "path" },
				{ name = "buffer" },
			},
			mapping = {
				["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
				["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
				["<C-y>"] = cmp.mapping(
					cmp.mapping.confirm {
						behavior = cmp.ConfirmBehavior.Insert,
						select = true,
					},
					{ "i", "c" }
				),
			},

			-- Enable luasnip to handle snippet expansion for nvim-cmp
			snippet = {
				expand = function(args)
					Require("luasnip").lsp_expand(args.body)
				end,
			},
		}
		end,
	}
}
