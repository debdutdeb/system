-- XXX: Note to self --
-- Lazy honestly feels bloated at this point.
-- Might not be the worst of ideas to strip those parts out
-- I like the idea of "lazy" loading plugins, but i don't want things like a floating terminal, or watching
-- config files to auto reload, or auto updating plugins (definitely not this, do I want to break my install every other day?).
-- Hm. Might not be the worst idea to go back to packer either.

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=main", -- latest stable release
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
		"ms-jpq/coq_nvim",
		branch = "coq",
		cmd = { "LspStartWithAutocomplete" },
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
		{
			'https://codeberg.org/esensar/nvim-dev-container',
			dependencies = 'nvim-treesitter/nvim-treesitter',
			lazy = false,
			config = function()
				require('devcontainer').setup({ container_runtime = "docker" })
			end,
		}
	},

	-- "lspcontainers/lspcontainers.nvim",

	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		ft = { "c", "cpp", "lua", "go", "typescript", "typescriptreact", "javascript", "javascriptreact", "rust", "markdown", "hcl", "terraform" },
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/playground",
			"nvim-treesitter/nvim-treesitter-refactor",
			"treesitter_context",
			"windwp/nvim-ts-autotag",
		},
		config = function()
			Require("nvim-treesitter.configs").setup(require("debdut.treesitter"))
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter-context",
		name = "treesitter_context",
		opts = require("debdut.treesitter_context"),
	},

	-- thanks primeagen (what is your real name??,
	{
		"ThePrimeagen/harpoon",
		keys = { "<leader>a", "<leader>h" },
	},

	-- remote containers?
	-- { "chipsenkbeil/distant.nvim", branch = "v0.2" },

	"towolf/vim-helm", -- TODO replace

	-- debug access protocol
	{
		"mfussenegger/nvim-dap", -- TODO
		dependencies = {
			"rcarriga/nvim-dap-ui", -- requires nim-dap
			"nvim-neotest/nvim-nio",
			"theHamsta/nvim-dap-virtual-text",
		},
		keys = { "<leader>db", "<leader>dc" },
		config = function()
			require("debdut.dap")
		end,
		lazy = false,
	},

	"nvim-neotest/nvim-nio",

	"JoosepAlviste/nvim-ts-context-commentstring",

	{
		"numToStr/Comment.nvim",
		dependencies = { "JoosepAlviste/nvim-ts-context-commentstring" },
		config = function()
			require('Comment').setup(require("debdut.comments"))
		end,
		keys = {
			{ "gcc", mode = "n" },
			{ "gc",  mode = "v" },
			{ "gb",  mode = "v" },
		}, -- default keymaps
	},

	{
		"wthollingsworth/pomodoro.nvim",
		cmd = { "PomodoroStart", "PomodoroStatus" },
		dependencies = { "MunifTanjim/nui.nvim" },
		opts = {
			time_work = 30,
			time_break_short = 5,
			time_break_long = 20,
			timers_to_long_break = 4,
		},
	},
	{
		"kylechui/nvim-surround",
		version = "v2.1.1",
		event = "VeryLazy",
		opts = {},
	},
	{
		"aserowy/tmux.nvim",
		event = "VeryLazy",
		opts = {
			copy_sync = {
				-- enables copy sync. by default, all registers are synchronized.
				-- to control which registers are synced, see the `sync_*` options.
				enable = true,

				-- ignore specific tmux buffers e.g. buffer0 = true to ignore the
				-- first buffer or named_buffer_name = true to ignore a named tmux
				-- buffer with name named_buffer_name :)
				ignore_buffers = { empty = false },

				-- TMUX >= 3.2: all yanks (and deletes) will get redirected to system
				-- clipboard by tmux
				redirect_to_clipboard = false,

				-- offset controls where register sync starts
				-- e.g. offset 2 lets registers 0 and 1 untouched
				register_offset = 0,

				-- overwrites vim.g.clipboard to redirect * and + to the system
				-- clipboard using tmux. If you sync your system clipboard without tmux,
				-- disable this option!
				sync_clipboard = true,

				-- synchronizes registers *, +, unnamed, and 0 till 9 with tmux buffers.
				sync_registers = true,

				-- syncs deletes with tmux clipboard as well, it is adviced to
				-- do so. Nvim does not allow syncing registers 0 and 1 without
				-- overwriting the unnamed register. Thus, ddp would not be possible.
				sync_deletes = true,

				-- syncs the unnamed register with the first buffer entry from tmux.
				sync_unnamed = true,
			},
			navigation = {
				-- cycles to opposite pane while navigating into the border
				cycle_navigation = true,

				-- enables default keybindings (C-hjkl) for normal mode
				enable_default_keybindings = true,

				-- prevents unzoom tmux when navigating beyond vim border
				persist_zoom = false,
			},
			resize = {
				-- enables default keybindings (A-hjkl) for normal mode
				enable_default_keybindings = true,

				-- sets resize steps for x axis
				resize_step_x = 1,

				-- sets resize steps for y axis
				resize_step_y = 1,
			},
		},
	},

	{
		"debdutdeb/nvim-fzf",
		lazy = false,
	},
	{
		"windwp/nvim-ts-autotag",
		ft = { "typescriptreact", "javascriptreact", "html" },
	},
	{
		"nvim-telescope/telescope.nvim",
		dependencies = { "debdutdeb/chaos.nvim", "nvim-telescope/telescope-fzf-native.nvim" },
		--[[ config = function()
			local telescope = require("telescope")
			telescope.setup(require("debdut.telescope"))
			telescope.load_extension("fzf")
		end, ]]
	},
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build =
		"cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
	},
	{
		"debdutdeb/chaos.nvim",
		lazy = false,
		dir = (function()
			if vim.uv.os_uname().sysname == "Linux" then
				return "/home/debdut/git/chaos.nvim"
			else
				return
				"/Users/debdut/Documents/Repos/chaos.nvim"
			end
		end)(),
		config = function()
			-- something is going on here, with telescope's action merge. idk what. some type of race condition because
			-- of lazy my guess is.
			-- for now ignoring the error is ok. and that
			-- is what i will be doing
			-- local config = require("debdut.lsp.configs")
			-- require("chaos.lsp").setup_autocommands(config.configured_servers, config.get_config)
			local telescope = Require("telescope")
			telescope.setup(Require("debdut.telescope"))
			telescope.load_extension("fzf")
		end,
	},
	{ "tpope/vim-abolish", lazy = false },
	{
		"folke/persistence.nvim",
		opts = {
			dir = vim.fn.expand(vim.fn.stdpath("state") .. "/sessions/"),
			options = { "buffers", "curdir", "tabpages", "winsize" },
			pre_save = nil,
			save_empty = false,
		},
		lazy = false,
	},
	{
		'stevearc/oil.nvim',
		opts = {
			-- Oil will take over directory buffers (e.g. `vim .` or `:e src/`)
			-- Set to false if you still want to use netrw.
			default_file_explorer = true,
			-- Id is automatically added at the beginning, and name at the end
			-- See :help oil-columns
			columns = {
				-- "icon",
				-- "permissions",
				-- "size",
				-- "mtime",
			},
			-- Buffer-local options to use for oil buffers
			buf_options = {
				buflisted = false,
				bufhidden = "hide",
			},
			-- Window-local options to use for oil buffers
			win_options = {
				wrap = false,
				signcolumn = "no",
				cursorcolumn = false,
				foldcolumn = "0",
				spell = false,
				list = false,
				conceallevel = 3,
				concealcursor = "nvic",
			},
			-- Send deleted files to the trash instead of permanently deleting them (:help oil-trash)
			delete_to_trash = false,
			-- Skip the confirmation popup for simple operations (:help oil.skip_confirm_for_simple_edits)
			skip_confirm_for_simple_edits = false,
			-- Selecting a new/moved/renamed file or directory will prompt you to save changes first
			-- (:help prompt_save_on_select_new_entry)
			prompt_save_on_select_new_entry = true,
			-- Oil will automatically delete hidden buffers after this delay
			-- You can set the delay to false to disable cleanup entirely
			-- Note that the cleanup process only starts when none of the oil buffers are currently displayed
			cleanup_delay_ms = 2000,
			lsp_file_methods = {
				-- Time to wait for LSP file operations to complete before skipping
				timeout_ms = 1000,
				-- Set to true to autosave buffers that are updated with LSP willRenameFiles
				-- Set to "unmodified" to only save unmodified buffers
				autosave_changes = false,
			},
			-- Constrain the cursor to the editable parts of the oil buffer
			-- Set to `false` to disable, or "name" to keep it on the file names
			constrain_cursor = "editable",
			-- Set to true to watch the filesystem for changes and reload oil
			experimental_watch_for_changes = false,
			-- Keymaps in oil buffer. Can be any value that `vim.keymap.set` accepts OR a table of keymap
			-- options with a `callback` (e.g. { callback = function() ... end, desc = "", mode = "n" })
			-- Additionally, if it is a string that matches "actions.<name>",
			-- it will use the mapping at require("oil.actions").<name>
			-- Set to `false` to remove a keymap
			-- See :help oil-actions for a list of all available actions
			keymaps = {
				["g?"] = "actions.show_help",
				["<CR>"] = "actions.select",
				["<C-s>"] = false,
				["<C-h>"] = false,
				["<leader>s"] = "actions.select_vsplit",
				["<leader>h"] = "actions.select_split",
				["<C-t>"] = "actions.select_tab",
				["<C-p>"] = "actions.preview",
				["<C-c>"] = "actions.close",
				["<leader>l"] = "actions.refresh",
				["<C-l>"] = false,
				["-"] = "actions.parent",
				["_"] = "actions.open_cwd",
				["`"] = "actions.cd",
				["~"] = "actions.tcd",
				["gs"] = "actions.change_sort",
				["gx"] = "actions.open_external",
				["g."] = "actions.toggle_hidden",
				["g\\"] = "actions.toggle_trash",
			},
			-- Configuration for the floating keymaps help window
			keymaps_help = {
				border = "none",
			},
			-- Set to false to disable all of the above keymaps
			use_default_keymaps = true,
			view_options = {
				-- Show files and directories that start with "."
				show_hidden = false,
				-- This function defines what is considered a "hidden" file
				is_hidden_file = function(name, bufnr)
					return vim.startswith(name, ".")
				end,
				-- This function defines what will never be shown, even when `show_hidden` is set
				is_always_hidden = function(name, bufnr)
					return false
				end,
				-- Sort file names in a more intuitive order for humans. Is less performant,
				-- so you may want to set to false if you work with large directories.
				natural_order = true,
				sort = {
					-- sort order can be "asc" or "desc"
					-- see :help oil-columns to see which columns are sortable
					{ "type", "asc" },
					{ "name", "asc" },
				},
			},
			-- Configuration for the floating window in oil.open_float
			float = {
				-- Padding around the floating window
				padding = 2,
				max_width = 0,
				max_height = 0,
				border = "none",
				win_options = {
					winblend = 0,
				},
				-- This is the config that will be passed to nvim_open_win.
				-- Change values here to customize the layout
				override = function(conf)
					return conf
				end,
			},
			-- Configuration for the actions floating preview window
			preview = {
				-- Width dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
				-- min_width and max_width can be a single value or a list of mixed integer/float types.
				-- max_width = {100, 0.8} means "the lesser of 100 columns or 80% of total"
				max_width = 0.9,
				-- min_width = {40, 0.4} means "the greater of 40 columns or 40% of total"
				min_width = { 40, 0.4 },
				-- optionally define an integer/float for the exact width of the preview window
				width = nil,
				-- Height dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
				-- min_height and max_height can be a single value or a list of mixed integer/float types.
				-- max_height = {80, 0.9} means "the lesser of 80 columns or 90% of total"
				max_height = 0.9,
				-- min_height = {5, 0.1} means "the greater of 5 columns or 10% of total"
				min_height = { 5, 0.1 },
				-- optionally define an integer/float for the exact height of the preview window
				height = nil,
				border = "none",
				win_options = {
					winblend = 0,
				},
				-- Whether the preview window is automatically updated when the cursor is moved
				update_on_cursor_moved = true,
			},
			-- Configuration for the floating progress window
			progress = {
				max_width = 0.9,
				min_width = { 40, 0.4 },
				width = nil,
				max_height = { 10, 0.9 },
				min_height = { 5, 0.1 },
				height = nil,
				border = "none",
				minimized_border = "none",
				win_options = {
					winblend = 0,
				},
			},
			-- Configuration for the floating SSH window
			ssh = {
				border = "none",
			},
		},
		--cmd = { "Oil" }
		lazy = false,
	},
	-- need to load this before dap, as asked, but
	-- since I am going to use this one's json decoder
	-- I need this to load before dap then force patch dap
	{
		'stevearc/overseer.nvim',
		lazy = false,
		opts = {
		},
	},
	{
		"stevearc/resession.nvim",
		lazy = true,
	},
}, {
	root = vim.fn.stdpath("data") .. "/lazy", -- directory where plugins will be installed
	defaults = {
		lazy = true,                       -- should plugins be lazy-loaded?
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
				Require("lazy.util").float_term({ "lazygit", "log" }, {
					cwd = plugin.dir,
				})
			end,

			-- open a terminal for the plugin dir
			["<localleader>t"] = function(plugin)
				Require("lazy.util").float_term(nil, {
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
		enabled = false,
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
			paths = {},  -- add any custom paths here that you want to includes in the rtp
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

vim.opt.rtp:prepend("/Users/debdut/Documents/Repos/chaos.nvim")
