local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"--single-branch",
		"https://github.com/folke/lazy.nvim.git",
		lazypath,
	})
end

local current_rtp = vim.api.nvim_list_runtime_paths()
vim.opt.runtimepath:prepend(lazypath)

local ok, lazy = pcall(require, "lazy")
if not ok then
	return
end

-- Install your plugins here
-- My plugins here

local PLUGINS = {
	{ "wbthomason/packer.nvim" }, -- Have packer manage itself
	{ "nvim-lua/plenary.nvim" }, -- Useful lua functions used by lots of plugins
	{ "lewis6991/impatient.nvim" },

	-- Colorschemes
	"EdenEast/nightfox.nvim",
	"savq/melange",
	"drewtempelmeyer/palenight.vim",
	"folke/tokyonight.nvim",
	"jacoborus/tender.vim",
	"rebelot/kanagawa.nvim",

	-- can't get rid of
	-- cmp plugins
	{ "hrsh7th/nvim-cmp" }, -- The completion plugin
	{ "hrsh7th/cmp-buffer" }, -- buffer completions
	{ "hrsh7th/cmp-path" }, -- path completions
	{ "saadparwaiz1/cmp_luasnip" }, -- snippet completions
	{ "hrsh7th/cmp-nvim-lsp" },
	{ "hrsh7th/cmp-nvim-lua" },

	-- can't get rid of
	-- snippets
	{ "L3MON4D3/LuaSnip" }, --snippet engine
	{ "rafamadriz/friendly-snippets" }, -- a bunch of snippets to use

	-- can't get rid of
	-- LSP
	{ "neovim/nvim-lspconfig" }, -- enable LSP
	{ "williamboman/mason.nvim" },
	{ "williamboman/mason-lspconfig.nvim" },
	{ "jose-elias-alvarez/null-ls.nvim" }, -- for formatters and linters

	-- Telescope
	{ "nvim-telescope/telescope.nvim" },
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
	},

	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
	},
	"nvim-treesitter/nvim-treesitter-context",

	-- thanks primeagen (what is your real name??,
	"ThePrimeagen/harpoon",

	-- remote containers?
	-- "chipsenkbeil/distant.nvim",

	"towolf/vim-helm", -- TODO replace

	-- debug access protocol
	"mfussenegger/nvim-dap", -- TODO

	"hashivim/vim-terraform", -- replace TODO
}

lazy.setup(PLUGINS)
