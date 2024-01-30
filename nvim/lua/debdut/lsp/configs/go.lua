return {
	cmd = { "gopls" },
	root_dir = Require('debdut.lsp.utils').root_directory_pattern({ "go.mod", ".git" }),
	settings = {
		gopls = {
			hints = {
				assignVariableTypes = true,
				compositeLiteralFields = true,
				constantValues = true,
				functionTypeParameters = true,
				parameterNames = true,
				rangeVariableTypes = true,
			},
		},
	},
}
