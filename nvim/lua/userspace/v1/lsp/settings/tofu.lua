local config = vim.lsp.config.terraformls
return vim.tbl_deep_extend('force', config, {
	filetypes = vim.tbl_extend('force', config.filetypes, {"terraform"}),
})
