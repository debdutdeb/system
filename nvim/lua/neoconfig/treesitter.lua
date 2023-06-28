local status_ok, configs = pcall(require, "nvim-treesitter.configs")
if not status_ok then
	return
end

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
	-- "markdown_inline",
	"zig",
}

configs.setup({
	ensure_installed = grammers, -- one of "all" or a list of languages
	-- the following is causing some issues :' )
	ignore_install = { "phpdoc" }, -- List of parsers to ignore installing
	highlight = {
		enable = true, -- false will disable the whole extension
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
		updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
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
})
