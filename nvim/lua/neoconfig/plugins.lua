local packer_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if not vim.loop.fs_stat(packer_path) then
	PACKER_BOOTSTRAP = vim.fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		packer_path,
	})
	print("Installing packer close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

local ok, packer = pcall(require, "packer")
if not ok then
	return
end

packer.init({
	display = {
		non_interactive = false,
		compact = true,
		open_fn = nil,
		open_cmd = "65vnew",
		working_sym = "[working]",
		error_sym = "[error]",
		done_sym = "[done]",
		removed_sym = "-",
		moved_sym = "[moved]",
		item_sym = "[item]",
		header_sym = "━",
		header_lines = 2,
		title = "manage neovim plugins",
		show_all_info = true,
		prompt_border = "none",
		keybindings = {
			quit = "q",
			toggle_update = "u",
			continue = "c",
			toggle_info = "<CR>",
			diff = "d",
			prompt_revert = "r",
			retry = "R",
		},
	},
	autoremove = true,
	compile_on_sync = true,
	auto_clean = true,
	ensure_dependencies = true,
}) -- TODO

local PLUGINS = {
	"wbthomason/packer.nvim", -- Have packer manage itself
	"nvim-lua/plenary.nvim", -- Useful lua functions used by lots of plugins
	"lewis6991/impatient.nvim",

	-- Colorschemes
	"EdenEast/nightfox.nvim",
	"savq/melange",
	"drewtempelmeyer/palenight.vim",
	"folke/tokyonight.nvim",
	"jacoborus/tender.vim",
	"rebelot/kanagawa.nvim",

	-- can't get rid of
	-- cmp plugins
	"hrsh7th/nvim-cmp", -- The completion plugin
	"hrsh7th/cmp-buffer", -- buffer completions
	"hrsh7th/cmp-path", -- path completions
	"saadparwaiz1/cmp_luasnip", -- snippet completions
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-nvim-lua",
	"hrsh7th/cmp-nvim-lsp-signature-help",
	"lvimuser/lsp-inlayhints.nvim",

	-- can't get rid of
	-- snip
	"L3MON4D3/LuaSnip", --snippet engine
	"rafamadriz/friendly-snippets", -- a bunch of snippets to use

	-- can't get rid of
	-- LSP
	"neovim/nvim-lspconfig", -- enable LSP
	"williamboman/mason.nvim",
	"williamboman/mason-lspconfig.nvim",
	"jose-elias-alvarez/null-ls.nvim", -- for formatters and linters
	"lspcontainers/lspcontainers.nvim",

	-- Telescope
	"nvim-telescope/telescope.nvim",
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		run = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
	},

	-- Treesitter
	"nvim-treesitter/nvim-treesitter",
	"nvim-treesitter/nvim-treesitter-context",
	"nvim-treesitter/playground",

	-- thanks primeagen (what is your real name??,
	"ThePrimeagen/harpoon",

	-- remote containers?
	{ "chipsenkbeil/distant.nvim", branch = "v0.2" },

	"towolf/vim-helm", -- TODO replace

	-- debug access protocol
	"mfussenegger/nvim-dap", -- TODO
	"leoluz/nvim-dap-go",
	"rcarriga/nvim-dap-ui", -- requires nim-dap
	"theHamsta/nvim-dap-virtual-text",
	"nvim-telescope/telescope-dap.nvim",
	"mxsdev/nvim-dap-vscode-js",
	{
		"microsoft/vscode-js-debug",
		opt = true,
		run = "npm install --legacy-peer-deps && npx gulp vsDebugServerBundle && mv dist out",
		tag = "v1.*",
	},
	"numToStr/Comment.nvim",
	"JoosepAlviste/nvim-ts-context-commentstring",

	"stevearc/oil.nvim",

	-- just for Kapply honestly
	-- { "rottencandy/vimkubectl", tag = "0.12.0" },
	"rottencandy/vimkubectl",
}

return packer.startup(function(use)
	for _, plugin in pairs(PLUGINS) do
		use(plugin)
	end
end)
