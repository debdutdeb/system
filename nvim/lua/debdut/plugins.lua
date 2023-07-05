local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

local ok, lazy = pcall(require, "lazy")
if not ok then
	return
end

lazy.setup({
	"nvim-lua/plenary.nvim", -- Useful lua functions used by lots of plugins
	{
		"lewis6991/impatient.nvim",
		priority = 1000,
		lazy = false,
		init = function()
			require("impatient").enable_profile()
		end,
	},

	-- Colorschemes
	{
		"EdenEast/nightfox.nvim",
		opts = require("debdut.nightfox"),
	},
	{
		"folke/tokyonight.nvim",
		opts = require("debdut.tokyonight"),
	},
	{
		"rebelot/kanagawa.nvim",
		opts = require("debdut.kanagawa"),
	},
	"savq/melange",
	"drewtempelmeyer/palenight.vim",
	"jacoborus/tender.vim",

	-- can't get rid of
	-- cmp plugins
	{
		"hrsh7th/nvim-cmp", -- The completion plugin
		event = "InsertEnter",
		dependencies = {
			"hrsh7th/cmp-buffer", -- buffer completions
			"hrsh7th/cmp-path", -- path completions
			"saadparwaiz1/cmp_luasnip", -- snippet completions
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-nvim-lua",
			"hrsh7th/cmp-nvim-lsp-signature-help",
			"L3MON4D3/LuaSnip", --snippet engine
			"rafamadriz/friendly-snippets", -- a bunch of snippets to userdata
		},
		config = function()
			require("debdut.cmp")
		end,
	},

	{
		"L3MON4D3/LuaSnip", --snippet engine
		build = "make install_jsregexp",
		init = function()
			require("luasnip.loaders.from_snipmate").lazy_load({ paths = "./snippets" })
		end,
	},

	-- can't get rid of
	-- LSP
	"neovim/nvim-lspconfig", -- enable LSP
	{
		"williamboman/mason.nvim",
		lazy = false,
		dependencies = {
			"williamboman/mason-lspconfig.nvim",
			--
			"neovim/nvim-lspconfig",
			--
			"nvim-lua/plenary.nvim",
		},
		config = function()
			require("mason").setup(require("debdut.lsp.mason"))
			require("mason-lspconfig").setup({
				ensure_installed = require("debdut.lsp.servers"),
				automatic_installation = true,
			})
			--  TODO maybe move this somewhere
			require("mason-nvim-dap").setup()
		end,
	},
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			"hrsh7th/nvim-cmp",
		},
		config = function()
			require("debdut.lsp")
		end,
	},
	{
		-- TODO: add ensure_installed
		"jay-babu/mason-null-ls.nvim",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			"williamboman/mason.nvim",
			"jose-elias-alvarez/null-ls.nvim",
		},
		config = function()
			require("debdut.null-ls")
			require("mason-null-ls").setup({ automatic_installation = false })
		end,
	},

	-- "lspcontainers/lspcontainers.nvim",

	-- Telescope
	{
		"nvim-telescope/telescope.nvim",
		config = function()
			local telescope = require("telescope")
			telescope.setup(require("debdut.telescope"))
			telescope.load_extension("fzf")
		end,
	},
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
		dependencies = {
			"nvim-telescope/telescope.nvim",
		},
	},

	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/playground",
			"nvim-treesitter/nvim-treesitter-refactor",
		},
		config = function()
			require("nvim-treesitter.configs").setup(require("debdut.treesitter"))
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter-context",
		opts = require("debdut.treesitter_context"),
	},

	-- thanks primeagen (what is your real name??,
	"ThePrimeagen/harpoon",

	-- remote containers?
	-- { "chipsenkbeil/distant.nvim", branch = "v0.2" },

	"towolf/vim-helm", -- TODO replace

	-- debug access protocol
	{
		"mfussenegger/nvim-dap", -- TODO
		dependencies = {
			"leoluz/nvim-dap-go",
			"rcarriga/nvim-dap-ui", -- requires nim-dap
			"theHamsta/nvim-dap-virtual-text",
			"nvim-telescope/telescope-dap.nvim",
		},
		config = function()
			require("debdut.dap")
		end,
	},
	{
		"jay-babu/mason-nvim-dap.nvim",
		dependencies = {
			"williamboman/mason.nvim",
			"mfussenegger/nvim-dap",
		},
	},

	{
		"JoosepAlviste/nvim-ts-context-commentstring",
	},

	{
		"numToStr/Comment.nvim",
		dependencies = { "JoosepAlviste/nvim-ts-context-commentstring" },
		opts = require("debdut.comments"),
	},

	{
		"stevearc/oil.nvim",
		opts = require("debdut.oil"),
	},

	-- just for Kapply honestly
	-- { "rottencandy/vimkubectl", tag = "0.12.0" },
	"rottencandy/vimkubectl",
}, {
	root = vim.fn.stdpath("data") .. "/lazy", -- directory where plugins will be installed
	defaults = {
		lazy = false, -- should plugins be lazy-loaded?
		version = nil,
		-- default `cond` you can use to globally disable a lot of plugins
		-- when running inside vscode for example
		cond = nil, ---@type boolean|fun(self:LazyPlugin):boolean|nil
		-- version = "*", -- enable this to try installing the latest stable versions of plugins
	},
	-- leave nil when passing the spec as the first argument to setup()
	spec = nil, ---@type LazySpec
	lockfile = vim.fn.stdpath("config") .. "/lazy-lock.json", -- lockfile generated after running update.
	-- concurrency = jit.os:find("Windows") and (vim.loop.available_parallelism() * 2) or nil, ---@type number limit the maximum amount of concurrent tasks
	git = {
		-- defaults for the `Lazy log` command
		-- log = { "-10" }, -- show the last 10 commits
		log = { "-8" }, -- show commits from the last 3 days
		timeout = 120, -- kill processes that take more than 2 minutes
		url_format = "https://github.com/%s.git",
		-- lazy.nvim requires git >=2.19.0. If you really want to use lazy with an older version,
		-- then set the below to false. This should work, but is NOT supported and will
		-- increase downloads a lot.
		filter = true,
	},
	--[[ dev = {
		-- directory where you store your local plugin projects
		path = "~/projects",
		---@type string[] plugins that match these patterns will use your local versions instead of being fetched from GitHub
		patterns = {}, -- For example {"folke"}
		fallback = false, -- Fallback to git when local plugin doesn't exist
	}, ]]
	install = {
		-- install missing plugins on startup. This doesn't increase startup time.
		missing = true,
		-- try to load one of these colorschemes when starting an installation during startup
		-- colorscheme = { "carbonfox" },
	},
	ui = {
		-- a number <1 is a percentage., >1 is a fixed size
		size = { width = 0.8, height = 0.8 },
		wrap = true, -- wrap the lines in the ui
		-- The border to use for the UI window. Accepts same border values as |nvim_open_win()|.
		border = "none",
		title = nil, ---@type string only works when border is not "none"
		title_pos = "right", ---@type "center" | "left" | "right"
		icons = {
			cmd = "cmd:",
			config = "cfg:",
			event = "event:",
			ft = "ft:",
			init = "init:",
			import = "import:",
			keys = "keys:",
			lazy = "lazy:",
			loaded = "loaded:",
			not_loaded = "not_loaded:",
			plugin = "plugin:",
			runtime = "runtime:",
			source = "source:",
			start = "start:",
			task = "task:",
			list = {
				"ball",
				"here",
				"star",
				"line",
			},
		},
		-- leave nil, to automatically select a browser depending on your OS.
		-- If you want to use a specific browser, you can define it here
		browser = nil, ---@type string?
		throttle = 20, -- how frequently should the ui process render events
		custom_keys = {
			-- you can define custom key maps here.
			-- To disable one of the defaults, set it to false

			-- open lazygit log
			["<localleader>l"] = function(plugin)
				require("lazy.util").float_term({ "lazygit", "log" }, {
					cwd = plugin.dir,
				})
			end,

			-- open a terminal for the plugin dir
			["<localleader>t"] = function(plugin)
				require("lazy.util").float_term(nil, {
					cwd = plugin.dir,
				})
			end,
		},
	},
	diff = {
		-- diff command <d> can be one of:
		-- * browser: opens the github compare view. Note that this is always mapped to <K> as well,
		--   so you can have a different command for diff <d>
		-- * git: will run git diff and open a buffer with filetype git
		-- * terminal_git: will open a pseudo terminal with git diff
		-- * diffview.nvim: will open Diffview to show the diff
		cmd = "git",
	},
	checker = {
		-- automatically check for plugin updates
		enabled = false,
		concurrency = nil, ---@type number? set to 1 to check for updates very slowly
		notify = true, -- get a notification when new updates are found
		frequency = 3600, -- check for updates every hour
	},
	change_detection = {
		-- automatically check for config file changes and reload the ui
		enabled = true,
		notify = true, -- get a notification when changes are found
	},
	performance = {
		cache = {
			enabled = true,
		},
		reset_packpath = true, -- reset the package path to improve startup time
		rtp = {
			reset = true, -- reset the runtime path to $VIMRUNTIME and your config directory
			---@type string[]
			paths = {}, -- add any custom paths here that you want to includes in the rtp
			---@type string[] list any plugins you want to disable here
			disabled_plugins = {
				-- "gzip",
				-- "matchit",
				-- "matchparen",
				-- "netrwPlugin",
				-- "tarPlugin",
				-- "tohtml",
				-- "tutor",
				-- "zipPlugin",
			},
		},
	},
	-- lazy can generate helptags from the headings in markdown readme files,
	-- so :help works even for plugins that don't have vim docs.
	-- when the readme opens with :help it will be correctly displayed as markdown
	readme = {
		enabled = true,
		root = vim.fn.stdpath("state") .. "/lazy/readme",
		files = { "README.md", "lua/**/README.md" },
		-- only generate markdown helptags for plugins that dont have docs
		skip_if_doc_exists = true,
	},
	state = vim.fn.stdpath("state") .. "/lazy/state.json", -- state info for checker and other things
	build = {
		-- Plugins can provide a `build.lua` file that will be executed when the plugin is installed
		-- or updated. When the plugin spec also has a `build` command, the plugin's `build.lua` not be
		-- executed. In this case, a warning message will be shown.
		warn_on_override = true,
	},
})
