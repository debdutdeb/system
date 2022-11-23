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
})
