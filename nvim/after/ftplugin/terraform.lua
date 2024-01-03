lsp.b.config = {
	cmd = { "terraform-ls", "serve" },
	root_dir = vim.uv.cwd(),
	single_file_support = true,
}

if vim.version().api_level > 9 then
	vim.treesitter.language.register("hcl", "terraform")
end
