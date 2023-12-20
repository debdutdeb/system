

local function root_directory_pattern(pattern)
	vim.validate({ _ = { pattern, "table" } })

	return vim.fs.dirname(vim.fs.find(pattern, { upward = true })[1]) or vim.fn.getcwd()
end


return { root_directory_pattern = root_directory_pattern }
