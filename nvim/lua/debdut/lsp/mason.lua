-- TODO: pcall
local plenary_path = require("plenary.path")

return {
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
