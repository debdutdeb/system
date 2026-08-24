			local ok, cmp = pcall(require, "cmp")
            if not ok then return end
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
					require("luasnip").lsp_expand(args.body)
				end,
			},
		}
