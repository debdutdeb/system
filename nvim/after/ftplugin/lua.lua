lsp.b.config = lsp.b.config + {
	cmd = { "lua-language-server" },
	root_dir = require('debdut.lsp.utils').root_directory_pattern({ ".luarc.json", ".luarc.jsonc", ".stylua.toml", "stylua.toml" }),
	single_file_support = true,
	log_level = vim.lsp.protocol.MessageType.Warning,
}
