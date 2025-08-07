-- luarocks install magick

return {
	"nvim-neo-tree/neo-tree.nvim",
	branch = "v3.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
		"MunifTanjim/nui.nvim",
		-- {"3rd/image.nvim", opts = {}}, -- Optional image support in preview window: See `# Preview Mode` for more information
	},
	lazy = false, -- neo-tree will lazily load itself
	---@module "neo-tree"
	---@type require('neo-tree').Config?
	opts = {
		-- fill any relevant options here
		border = "none",
		window = {
			position = "right",
		},
		filesystem = {
			-- https://github.com/nvim-neo-tree/neo-tree.nvim/blob/5224467c6a49a6c77b8a8333f4d9af0abc788e10/lua/neo-tree/setup/netrw.lua#L14
			hijack_netrw_behavior = "open_current" -- https://github.com/nvim-neo-tree/neo-tree.nvim/blob/46fa0c22ca39e05fe15744102d21feb07fe9a94a/lua/neo-tree/setup/init.lua#L622
		}
	},
}
