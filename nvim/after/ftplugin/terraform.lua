if vim.version().api_level > 9 then
	vim.treesitter.language.register("hcl", "terraform")
end
