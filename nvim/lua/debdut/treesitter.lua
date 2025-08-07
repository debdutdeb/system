local grammers = {
	"c",
	"tsx",
	"javascript",
	"bash",
	"go",
	"rust",
	"typescript",
	"cpp",
	"jsonc", -- for devcontainers
	"vim",
	-- "help",
	"vimdoc",
	"hcl",
	"comment", -- for todo fixme etc
	"markdown",
	"markdown_inline",
	"zig",
	"lua",
	"html",
	"org",
	"dockerfile",
	"toml",
}

local plenary_path = require("plenary.path")

local parser_install_dir =
	plenary_path:new(vim.fn.stdpath("data")):joinpath(plenary_path:new("treesitter_parsers")):absolute()

-- wow: ~append~ -> prepend
-- reference: https://github.com/nvim-treesitter/nvim-treesitter/issues/3092
-- Specifically (quoting https://github.com/clason):
-- UPDATE Since 0.8.0, Neovim bundles parsers and queries for c, lua, vim, and help. If you use nvim-treesitter, you must make sure these parsers are installed via nvim-treesitter so that both parser and queries (which are always installed) are taken from nvim-treesitter. (It's important for this that your nvim-treesitter plugin directory comes before both /usr/local/share/nvim/* and /usr/lib/nvim in your runtimepath.)
vim.opt.runtimepath:prepend(parser_install_dir)

return {
	parser_install_dir = parser_install_dir,
	ensure_installed = grammers, -- one of "all" or a list of languages
	-- the following is causing some issues :' )
	ignore_install = { "phpdoc", "org" }, -- List of parsers to ignore installing
	highlight = {
		enable = true,          -- false will disable the whole extension
		additional_vim_regex_highlighting = true,
	},
	autopairs = {
		enable = false, -- nyah
	},
	autotag = {
		enable = true, -- https://github.com/windwp/nvim-ts-autotag
		enable_rename = true,
		enable_close = true,
		enable_close_on_slash = true,
		filetypes = {
			"typescriptreact", "javascriptreact", "html", "xml" },
	},
	indent = {
		enable = true,
		disable = { --[["python", "css"--]]
		},
	},
	playground = {
		enable = true,
		disable = {},
		updatetime = 25,   -- Debounced time for highlighting nodes in the playground from source code
		persist_queries = false, -- Whether the query persists across vim sessions
		keybindings = {
			toggle_query_editor = "o",
			toggle_hl_groups = "i",
			toggle_injected_languages = "t",
			toggle_anonymous_nodes = "a",
			toggle_language_display = "I",
			focus_language = "f",
			unfocus_language = "F",
			update = "R",
			goto_node = "<cr>",
			show_help = "?",
		},
	},
	refactor = {
		navigation = {
			enable = true,
			keymaps = {
				goto_definition = false,
				list_definitions = false,
				list_definitions_toc = false,
				goto_next_usage = "<C-n>",
				goto_previous_usage = "<C-p>",
			},
		},
	},
	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "gnn", -- starts nexts
			node_incremental = "grn",
			scope_incremental = "grc",
			node_decremental = "grm",
		},
	},
}
