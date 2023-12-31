local root_files = {
	'pyproject.toml',
	'setup.py',
	'setup.cfg',
	'requirements.txt',
	'Pipfile',
	'pyrightconfig.json',
	'.git',
}

lsp.b.config = {
	cmd = { "pyright-langserver", "--stdio" },
	root_dir = require('debdut.lsp.utils').root_directory_pattern(root_files),
	single_file_support = true,
	settings = {
		python = {
			analysis = {
				autoSearchPaths = true,
				useLibraryCodeForTypes = true,
				diagnosticMode = 'openFilesOnly',
			},
			pythonPath = "/usr/local/bin/python3",
		},
	},
}
