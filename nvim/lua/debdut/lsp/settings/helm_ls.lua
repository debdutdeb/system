local settings = {
	default_config = {
		cmd = { "helm_ls", "serve" },
		filetypes = { "helm" },
		root_dir = function(fname)
			return util.root_pattern("Chart.yaml")(fname)
		end,
	},
	filetypes = { "helm" },
	cmd = { "helm_ls", "serve" },
}

return settings
