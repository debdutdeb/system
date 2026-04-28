-- All plugins: runtimepath in fixed order, then one startup-time setup chain
-- (no plugin manager, no lazy-loading).
-- Under Nix: NIX_NVIM_PLUGIN_DIRS points to a flake-generated path map; each
-- plugin is loaded from the Nix store.  Without Nix: use ~/.local/share/nvim/lazy/<name>
-- (the same directory layout the old lazy.nvim install produced).

---@param map table<string, string> key -> path
---@param order string[]
local function apply_rtp(map, order)
	for _, key in ipairs(order) do
		local p = map[key]
		if p and vim.uv.fs_stat(p) then
			vim.opt.rtp:prepend(p)
		end
	end
end

-- Keys in prepend order.  Must match the generated Nix file (RTP map keys) and
-- the fallback `lazy/` folder names in path_from_lazy below.
local RTP_ORDER = {
	"plenary",
	"nvimNio",
	"nvimWebDevicons",
	"nui",
	"luaEvents",
	"hybridNvim",
	"neodev",
	"schemastore",
	"mason",
	"masonLspconfig",
	"masonNullLs",
	"noneLs",
	"noneLsExtras",
	"nvimTsContextCommentstring",
	"commentNvim",
	"luasnip",
	"cmpNvimLsp",
	"cmpPath",
	"cmpBuffer",
	"cmpLuasnip",
	"nvimCmp",
	"nvimTreesitterRefactor",
	"nvimTsAutotag",
	"nvimTreesitter",
	"treesitterContext",
	"playground",
	"lspSignature",
	"nvimLspconfig",
	"persistence",
	"neovimSessionManager",
	"neovimProject",
	"fzfLua",
	"telescopeFzfNative",
	"telescope",
	"telescopeOrgmode",
	"chaosNvim",
	"vimFugitive",
	"gitsigns",
	"overseer",
	"nvimDap",
	"nvimDapVirtualText",
	"nvimDapUi",
	"dapVscodeJs",
	"nvimSurround",
	"harpoon",
	"mongoNvim",
	"neoTree",
	"oil",
	"orgBullets",
	"orgmode",
	"pomodoro",
	"presenting",
	"tmuxNvim",
	"vimAbolish",
	"vimGhost",
	"vimTableMode",
}

---@param key string
---@return string|nil
local function path_from_lazy(key)
	--- lazy-lock.json keys and RTP_ORDER keys
	local T = {
		plenary = "plenary.nvim",
		nvimNio = "nvim-nio",
		nvimWebDevicons = "nvim-web-devicons",
		nui = "nui.nvim",
		luaEvents = "lua-events",
		hybridNvim = "hybrid.nvim",
		neodev = "neodev.nvim",
		schemastore = "SchemaStore.nvim",
		mason = "mason.nvim",
		masonLspconfig = "mason-lspconfig.nvim",
		masonNullLs = "mason-null-ls.nvim",
		noneLs = "none-ls.nvim",
		noneLsExtras = "none-ls-extras.nvim",
		nvimTsContextCommentstring = "nvim-ts-context-commentstring",
		commentNvim = "Comment.nvim",
		luasnip = "LuaSnip",
		cmpNvimLsp = "cmp-nvim-lsp",
		cmpPath = "cmp-path",
		cmpBuffer = "cmp-buffer",
		cmpLuasnip = "cmp_luasnip",
		nvimCmp = "nvim-cmp",
		nvimTreesitterRefactor = "nvim-treesitter-refactor",
		nvimTsAutotag = "nvim-ts-autotag",
		nvimTreesitter = "ts",
		treesitterContext = "treesitter-context",
		playground = "playground",
		lspSignature = "lsp_signature.nvim",
		nvimLspconfig = "nvim-lspconfig",
		persistence = "persistence.nvim",
		neovimSessionManager = "neovim-session-manager",
		neovimProject = "neovim-project",
		fzfLua = "fzf-lua",
		telescopeFzfNative = "telescope-fzf-native.nvim",
		telescope = "telescope.nvim",
		telescopeOrgmode = "telescope-orgmode.nvim",
		chaosNvim = "chaos.nvim",
		vimFugitive = "vim-fugitive",
		gitsigns = "gitsigns.nvim",
		overseer = "overseer.nvim",
		nvimDap = "nvim-dap",
		nvimDapVirtualText = "nvim-dap-virtual-text",
		nvimDapUi = "nvim-dap-ui",
		dapVscodeJs = "dap-vscode-js",
		nvimSurround = "nvim-surround",
		harpoon = "harpoon",
		mongoNvim = "mongo.nvim",
		neoTree = "neo-tree.nvim",
		oil = "oil.nvim",
		orgBullets = "org-bullets.nvim",
		orgmode = "orgmode",
		pomodoro = "pomodoro.nvim",
		presenting = "presenting.nvim",
		tmuxNvim = "tmux.nvim",
		vimAbolish = "vim-abolish",
		vimGhost = "vim-ghost",
		vimTableMode = "vim-table-mode",
	}
	local sub = T[key]
	if not sub then
		return nil
	end
	return vim.fn.stdpath("data") .. "/lazy/" .. sub
end

---@return table<string, string>
local function nix_path_map()
	local f = vim.env.NIX_NVIM_PLUGIN_DIRS
	if f and f ~= "" and vim.uv.fs_stat(f) then
		local ok, t = pcall(dofile, f)
		-- Reject the old lazy.nvim merge file (list of { "owner/repo", dir = "..." }).
		if ok and type(t) == "table" and type(t.plenary) == "string" then
			return t
		end
		vim.notify(
			"[plugins] NIX_NVIM_PLUGIN_DIRS is missing or not the flat path map; using ~/.local/share/nvim/lazy/ fallback",
			vim.log.levels.WARN
		)
	end
	local m = {}
	for _, key in ipairs(RTP_ORDER) do
		m[key] = path_from_lazy(key)
	end
	return m
end

local plugin_paths = nix_path_map()
apply_rtp(plugin_paths, RTP_ORDER)

if not plugin_paths.plenary or vim.uv.fs_stat(plugin_paths.plenary) == nil then
	vim.notify(
		"[plugins] Plugin tree not found. Rebuild with `nix build` (Nix) or keep ~/.local/share/nvim/lazy/ as before.",
		vim.log.levels.WARN
	)
	return
end

-- Project init (neovim-project) — sessionoptions before project setup
vim.opt.sessionoptions:append("globals")
if not pcall(vim.cmd.colorscheme, "hybrid") then
	vim.cmd.colorscheme("default")
end

-- persistence.nvim
do
	if vim.g.vscode == nil then
		local sep = "/"
		if vim.fn.has("win32") == 1 then
			sep = "[\\:]"
		end
		local origin = vim.trim(vim.fn.system("git remote get-url origin")):gsub(sep, "%%")
		local path_suffix
		if vim.v.shell_error ~= 0 then
			path_suffix = ""
		else
			local branch = vim.trim(vim.fn.system("git branch --show-current"))
			if vim.v.shell_error ~= 0 then
				path_suffix = origin .. "/"
			else
				path_suffix = origin .. "%%" .. branch .. "/"
			end
		end
		require("persistence").setup({
			dir = vim.fn.expand(vim.fn.stdpath("state") .. "/sessions/") .. path_suffix,
			options = { "buffers", "curdir", "tabpages", "winsize" },
			pre_save = nil,
			save_empty = false,
		})
	end
end

-- neovim-project
do
	require("neovim-project").setup({
		projects = { "~/git/*" },
		picker = { type = "telescope" },
		last_session_on_startup = false,
		dashboard_mode = true,
	})
end

-- treesitter
require("nvim-treesitter.configs").setup(require("debdut.treesitter"))
require("treesitter-context").setup(require("debdut.treesitter_context"))
pcall(function()
	require("nvim-ts-autotag").setup()
end)


-- Comment
require("Comment").setup(require("debdut.comments"))

-- nvim-cmp + cmp_nvim_lsp: cmp.setup first, then require("cmp_nvim_lsp").setup() so the
-- `nvim_lsp` source is actually registered (cmp.register_source on InsertEnter). Only
-- `require("cmp_nvim_lsp")` is not enough — the after/plugin script may not run with Nix rtp.
local cmp_nvim_lsp = require("cmp_nvim_lsp")
do
	local cmp = require("cmp")
	local ok_cmp_luasnip = pcall(require, "cmp_luasnip")
	local sources = {
		{ name = "nvim_lsp" },
		{ name = "path" },
		{ name = "buffer" },
	}
	if ok_cmp_luasnip then
		table.insert(sources, 2, { name = "luasnip" })
	end
	cmp.setup({
		completion = {
			completeopt = "menu,menuone,noselect",
		},
		sources = sources,
		mapping = cmp.mapping.preset.insert({
			["<C-Space>"] = cmp.mapping.complete(),
			["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
			["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
			["<C-y>"] = cmp.mapping(
				cmp.mapping.confirm {
					behavior = cmp.ConfirmBehavior.Insert,
					select = true,
				},
				{ "i", "c" }
			),
		}),
		snippet = {
			expand = function(args)
				require("luasnip").lsp_expand(args.body)
			end,
		},
	})
	-- Registers InsertEnter → register each LSP client as `nvim_lsp` source (required).
	cmp_nvim_lsp.setup()
end

require("debdut.lsp")

-- telescope + fzf native
do
	local tel = require("telescope")
	tel.setup(require("debdut.telescope"))
	pcall(tel.load_extension, "fzf")
end

-- chaos
require("chaos").setup_commands()

-- gitsigns + fugitive (only in a git worktree, same as previous cond)
do
	local ok, git = pcall(require, "chaos.git_handlers")
	local in_git = ok and git.is_git_worktree()
	if in_git then
		require("gitsigns").setup({
			signcolumn = false,
			numhl = false,
			linehl = false,
			word_diff = false,
			watch_gitdir = { follow_files = false },
			auto_attach = false,
			attach_to_untracked = false,
			current_line_blame = false,
			current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d> - <summary>",
		})
		-- fugitive: Vim plugin, no setup
	end
end

-- overseer, then dap
require("overseer").setup({ dap = false })
require("debdut.dap")

-- JS/TS dap
do
	local dap = require("dap")
	local dap_vscode_js = require("dap-vscode-js")
	dap_vscode_js.setup({
		debugger_path = vim.g.vscode_js_debug_path
			or (vim.fn.stdpath("data") .. "/lazy/vscode-js-debug"),
		adapters = {
			"chrome",
			"pwa-node",
			"pwa-chrome",
			"pwa-msedge",
			"node-terminal",
			"pwa-extensionHost",
			"node",
			"chrome",
		},
	})
	for _, language in
		ipairs({
			"javascript",
			"typescript",
			"javascriptreact",
			"typescriptreact",
			"tsx",
			"jsx",
		})
	do
		dap.configurations[language] = {
			{
				type = "pwa-node",
				request = "attach",
				name = "Attach",
				skipFiles = { "<node_internals>/**" },
				port = 9229,
			},
		}
	end
end

-- surround
require("nvim-surround").setup({})

-- harpoon
require("debdut.harpoon")

-- oil
do
	require("oil").setup {
		default_file_explorer = false,
		columns = {},
		buf_options = {
			buflisted = false,
			bufhidden = "hide",
		},
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
		delete_to_trash = false,
		skip_confirm_for_simple_edits = true,
		prompt_save_on_select_new_entry = false,
		cleanup_delay_ms = 2000,
		lsp_file_methods = { timeout_ms = 1000, autosave_changes = false },
		constrain_cursor = "editable",
		experimental_watch_for_changes = false,
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
		keymaps_help = { border = "none" },
		use_default_keymaps = true,
		view_options = {
			show_hidden = false,
			is_hidden_file = function(name, _)
				return vim.startswith(name, ".")
			end,
			is_always_hidden = function()
				return false
			end,
			natural_order = true,
			sort = { { "type", "asc" }, { "name", "asc" } },
		},
		float = {
			padding = 2,
			max_width = 0,
			max_height = 0,
			border = "none",
			win_options = { winblend = 0 },
			override = function(conf)
				return conf
			end,
		},
		preview = {
			max_width = 0.9,
			min_width = { 40, 0.4 },
			width = nil,
			max_height = 0.9,
			min_height = { 5, 0.1 },
			height = nil,
			border = "none",
			win_options = { winblend = 0 },
			update_on_cursor_moved = true,
		},
		progress = {
			max_width = 0.9,
			min_width = { 40, 0.4 },
			width = nil,
			max_height = { 10, 0.9 },
			min_height = { 5, 0.1 },
			height = nil,
			border = "none",
			minimized_border = "none",
			win_options = { winblend = 0 },
		},
		ssh = { border = "none" },
	}
	vim.keymap.set("n", "<leader>e", "<cmd>Oil<cr>")
end

-- neo-tree
do
	require("neo-tree").setup({
		border = "none",
		window = { position = "right" },
		filesystem = { hijack_netrw_behavior = "open_current" },
	})
end

-- mongodb
do
	require("mongo").setup {
		default_url = "mongodb://localhost:27017",
		find_on_collection_selected = false,
	}
end

-- pomodoro
do
	require("pomodoro").setup({
		time_work = 30,
		time_break_short = 5,
		time_break_long = 20,
		timers_to_long_break = 4,
	})
end

-- presenting.nvim
do
	local Presenting = require("presenting")
	Presenting.setup({
		options = {
			width = math.ceil(vim.api.nvim_win_get_width(0) * 0.75),
		},
		separator = {
			markdown = "^#+ ",
			org = "^*+ ",
			adoc = "^==+ ",
			asciidoctor = "^==+ ",
		},
		keymaps = {
			["n"] = function()
				Presenting.next()
			end,
			["p"] = function()
				Presenting.prev()
			end,
			["q"] = function()
				Presenting.quit()
			end,
			["f"] = function()
				Presenting.first()
			end,
			["l"] = function()
				Presenting.last()
			end,
			["<CR>"] = function()
				Presenting.next()
			end,
			["<BS>"] = function()
				Presenting.prev()
			end,
		},
	})
end

-- tmux
do
	require("tmux").setup({
		copy_sync = {
			enable = true,
			ignore_buffers = { empty = false },
			redirect_to_clipboard = false,
			register_offset = 0,
			sync_clipboard = true,
			sync_registers = true,
			sync_deletes = true,
			sync_unnamed = true,
		},
		navigation = { cycle_navigation = true, enable_default_keybindings = true, persist_zoom = false },
		resize = { enable_default_keybindings = true, resize_step_x = 1, resize_step_y = 1 },
	})
end
