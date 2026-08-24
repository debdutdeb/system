---@param specs table<string, string>
local function pack_build_initialize(specs)
    vim.api.nvim_create_autocmd("PackChanged", {
        callback = function(ev)
            local name = ev.data.spec.name
            local kind = ev.data.kind
            
            if kind == 'install' or kind == 'update' then
                if specs[name] ~= nil then
                    vim.system({ "sh", "-c", specs[name] }, {
                        cwd = ev.data.path,
                    }):wait()
                end
            end
        end,
    })
end

---@class InnerPluginSpec
---@field name string
---@field src string
---@field build? string
---@field condition? function
---@field version? vim.version.VersionRange | string
---@field config? function

---@class InnerPluginData
---@field spec {data: InnerPluginSpec, src: string, name: string}

---@param plug_data InnerPluginData
local function _load(plug_data)
    local maybe_load = (plug_data.spec.data or {}).condition
    if vim.is_callable(maybe_load) then
        if maybe_load() then vim.cmd.packadd(plug_data.spec.name) else return end
    else
        vim.cmd.packadd(plug_data.spec.name)
    end
    local maybe_config = (plug_data.spec.data or {}).config
    if vim.is_callable(maybe_config) then maybe_config() end
end

---@param all (InnerPluginSpec | string)[]
local function pack_add(all)
    local github_prefix = "https://github.com/"
    local plugins = {}
    ---@type table<string, string>
    local init_specs = {}
    for _, plugin in ipairs(all) do
        if type(plugin) == 'table' then
            local spec = vim.deepcopy(plugin)
            spec.src = github_prefix .. spec.src
            spec.data = spec
            table.insert(plugins, spec)
            
            if spec.build then
                init_specs[spec.name] = spec.build
            end
        else
            plugin = github_prefix .. plugin
            table.insert(plugins, plugin)
        end
    end
    pack_build_initialize(init_specs)
    vim.pack.add(plugins, { load = _load })
end

---@param plugin InnerPluginSpec
---@param enabled boolean
---@return InnerPluginSpec
local function colorscheme(plugin, enabled)
    if enabled and not vim.is_callable(plugin.config) then
        plugin.config = function ()
            vim.cmd("colorscheme " .. plugin.name)
        end
    end
    return plugin
end

pack_add({
    "nvim-lua/plenary.nvim",
    "debdutdeb/chaos.nvim",
    -- TREESITTER
    {
        src = 'nvim-treesitter/nvim-treesitter', 
        name = 'treesitter',
        version = '8b98b4470eb326f1c7b50dae79f8c963568e5720' -- v0.10.0 is over a year old, let's just walk a core plugin commits by hand
    },
    -- allow plain pinning
    -- FIXME: this isn't working, nvim-treesitter.query is not found or something
    -- "nvim-treesitter/nvim-treesitter-refactor",
    "nvim-treesitter/nvim-treesitter-context",
    -- FIXME: also failing
    -- "nvim-treesitter/playground",
    "windwp/nvim-ts-autotag",
    "ray-x/lsp_signature.nvim", -- TODO: add filestypes-that-need-code-things
    

    -- TELESCOPE
   "nvim-telescope/telescope.nvim",
   { src = "nvim-telescope/telescope-fzf-native.nvim", name = "telescope-fzf-native", build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build" },
   
   -- co,pletions
    "hrsh7th/nvim-cmp",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-buffer",
    { src = "L3MON4D3/LuaSnip", build = "make install_jsregexp" , name = "luasnip" },
    "saadparwaiz1/cmp_luasnip",
    
    {
        src = "debdutdeb/nvim-fzf",
        name = "mine-fzf",
        condition = function ()
            local loaded, _ = pcall(require, 'telescope')
            return not loaded
        end
    },
    
    --lsp
    "neovim/nvim-lspconfig",
    "folke/neodev.nvim",
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",
	"jay-babu/mason-null-ls.nvim",
    -- Schema information
    "b0o/SchemaStore.nvim",
    
	"nvimtools/none-ls.nvim",
	"jay-babu/mason-null-ls.nvim",
	"nvimtools/none-ls.nvim",
	"nvimtools/none-ls-extras.nvim",

    { src = "folke/persistence.nvim", name = "persistence", condition = function ()
        return vim.g.vscode == nil
    end },
    
    -- FIXME(api_level=14): BufLeave Autocommands for "*": Vim(append):Lua callback: Vim:E474: Error while dumping encode_tv2json() argument, key 'refresh_projects_b4update': attempt to dump
    --  function reference
    -- stack traceback:
    --         [C]: in function 'json_encode'
    --         ...are/nvim/site/pack/core/opt/harpoon/lua/harpoon/init.lua:174: in function 'save'
    --         ...are/nvim/site/pack/core/opt/harpoon/lua/harpoon/mark.lua:18: in function 'emit_changed'
    --         ...are/nvim/site/pack/core/opt/harpoon/lua/harpoon/mark.lua:281: in function 'store_offset'
    --         ...are/nvim/site/pack/core/opt/harpoon/lua/harpoon/init.lua:20: in function <...are/nvim/site/pack/core/opt/harpoon/lua/harpoon/init.lua:19>
    --         [C]: at 0x0102e157b4
    --         [C]: in function 'pcall'
    --         .../share/nvim/site/pack/core/opt/oil.nvim/lua/oil/init.lua:768: in function 'callback'
    --         ...m/site/pack/core/opt/oil.nvim/lua/oil/adapters/files.lua:279: in function ''
    --         vim/_core/editor.lua: in function <vim/_core/editor.lua:0>
    -- "ThePrimeagen/harpoon",
	'stevearc/oil.nvim',
	"aserowy/tmux.nvim",
    "tpope/vim-abolish",
    
    {
        src = "tpope/vim-fugitive",
        name = "fugitive",
		condition = function()
			local ok, git = pcall(require, "chaos.git_handlers")
			if not ok then return false end
			return git.is_git_worktree()
			--[[ return #vim.fs.find('.git',
				{ upward = true, type = 'directory', limit = 1, stop = vim.uv.os_homedir(), path = vim.fs.dirname(vim
				.api.nvim_buf_get_name(0)), }) == 1 ]]
		end,
    },
	{
		src = "lewis6991/gitsigns.nvim",
        name = "gitsigns",
		condition = function()
			local ok, git = pcall(require, "chaos.git_handlers")
			if not ok then return false end
			return git.is_git_worktree()
		end,
	},
    {
        src = "kylechui/nvim-surround",
        version = "v2.1.1",
        name = "surround",
    },

    
	colorscheme({ src = 'savq/melange-nvim', name = "melagne", }, false),
	colorscheme({ src = 'JoosepAlviste/palenightfall.nvim', name = "palenightfall", config = function()
        require 'palenightfall'.setup { transparent = true }
        vim.cmd 'colorscheme palenightfall'
    end, condition = function ()
        return false
    end }, false),
	colorscheme({
		src = "HoNamDuong/hybrid.nvim",
        name = "hybrid",
	}, true),
})

require("chaos").setup_commands()

require("nvim-treesitter.config").setup(require("userspace.v1.treesitter"))
require("userspace.v1.treesitter_context")

require("userspace.v1.telescope")

require "userspace.v1.completions"

require 'userspace.v1.lsp'
require 'userspace.v1.null-ls'
require 'userspace.v1.persistence'
-- require'userspace.v1.harpoon'
require'userspace.v1.oil'
require'userspace.v1.tmux'

require'userspace.v1.git'
