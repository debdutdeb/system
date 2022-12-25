local status_ok, mason = pcall(require, "mason")
if not status_ok then
	return
end
local mason_lsp_config
status_ok, mason_lsp_config = pcall(require, "mason-lspconfig")
if not status_ok then
	return
end

local lspconfig = require("lspconfig")
local plenary_path = require("plenary.path")

local DEFAULT_SETTINGS = {
	install_root_dir = plenary_path:new(vim.fn.stdpath("data")):joinpath(plenary_path:new("mason")):absolute(),
	PATH = "prepend",
	pip = {
		upgrade_pip = false,
		install_args = {},
	},
	log_level = vim.log.levels.INFO,
	max_concurrent_installers = 4,
	github = {
		download_url_template = "https://github.com/%s/releases/download/%s/%s",
	},
	providers = {
		"mason.providers.registry-api",
	},
	ui = {
		check_outdated_packages_on_open = false,
		border = "none",
		icons = {
			package_installed = "[installed]",
			package_pending = "[pending]",
			package_uninstalled = "[not installed]",
		},
		keymaps = {
			toggle_package_expand = "<CR>",
			install_package = "i",
			update_package = "u",
			check_package_version = "c",
			update_all_packages = "U",
			check_outdated_packages = "C",
			uninstall_package = "X",
			cancel_installation = "<C-c>",
			apply_language_filter = "<C-f>",
		},
	},
}

local servers = {
	"yamlls",
	"bashls",
	"jsonls",
	"sumneko_lua",
	"tsserver",
	"gopls",
	"dockerls",
	"solargraph",
	"clangd",
	"rust_analyzer",
	"marksman",
	"html",
	"kotlin_language_server",
	"jdtls",
	"perlnavigator",
	"pyright",
	"intelephense",
	"terraformls",
}

mason.setup(DEFAULT_SETTINGS)

mason_lsp_config.setup({
	ensure_installed = servers,
	automatic_installation = false,
})

local lsp_handlers = require("neoconfig.lsp.handlers")

for _, server in pairs(servers) do
	local opts = {
		on_attach = lsp_handlers.on_attach,
		capabilities = lsp_handlers.capabilities,
	}
	local has_custom_opts, server_custom_opts = pcall(require, "neoconfig.lsp.settings." .. server)
	if has_custom_opts then
		opts = vim.tbl_deep_extend("force", opts, server_custom_opts)
	end
	lspconfig[server].setup(opts)
end
