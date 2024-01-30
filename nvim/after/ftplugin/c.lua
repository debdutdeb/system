lsp.b.config = {
	cmd = {"clangd"},
	root_dir = vim.uv.cwd(),
	single_file_support = true,
	on_attach = function(client, bufnr)
		client.server_capabilities.document_formatting = false
		Require('debdut.lsp.base_config').on_attach(client, bufnr)
	end,
}
