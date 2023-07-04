local configs = require("lspconfig.configs")
local util = require("lspconfig.util")

configs.helm_ls = {
	default_config = {
		cmd = { "helm_ls", "serve" },
		filetypes = { "helm" },
		root_dir = function(fname)
			return util.root_pattern("Chart.yaml")(fname)
		end,
	},
}
