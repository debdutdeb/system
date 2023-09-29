local grammers = {
	"tsx",
	"javascript",
	"bash",
	"go",
	"rust",
	"typescript",
	"cpp",
	"jsonc", -- for devcontainers
	"query",
	"vim",
	-- "help",
	"hcl",
	"comment", -- for todo fixme etc
	"markdown",
	"markdown_inline",
	"zig",
}

local plenary_path = require("plenary.path")

local parser_install_dir =
	plenary_path:new(vim.fn.stdpath("data")):joinpath(plenary_path:new("treesitter_parsers")):absolute()

vim.opt.runtimepath:append(parser_install_dir)

return {
	parser_install_dir = parser_install_dir,
	ensure_installed = grammers, -- one of "all" or a list of languages
	-- the following is causing some issues :' )
	ignore_install = { "phpdoc" }, -- List of parsers to ignore installing
	highlight = {
		enable = true,          -- false will disable the whole extension
		additional_vim_regex_highlighting = true,
	},
	autopairs = {
		enable = false, -- nyah
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
				goto_next_usage = "<C-a>",
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

