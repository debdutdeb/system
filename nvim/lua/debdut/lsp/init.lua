--[[ https://github.com/neovim/nvim-lspconfig/tree/master/lua/lspconfig/server_configurations ]]

vim.diagnostic.config({
	virtual_text = true,
	update_in_insert = true,
	underline = true,
	severity_sort = true,
	float = {
		source = "if_many",
		header = "",
		prefix = "",
	},
})

-- https://github.com/neovim/neovim/issues/12544#issuecomment-1116794687

_G.lsp = { b = setmetatable({}, {
	__index = function(self, k)
		vim.validate({_ = {k, {'s', 'n'}}})
		if type(k) == 'number' then
			return rawget(self, k)
		end

		local bufnr = vim.api.nvim_get_current_buf()
		return rawget(rawget(self, bufnr), k)
	end,

	__newindex = function(self, k, v)
		vim.validate({_ = {k, 's'}})
		local bufnr =vim.api.nvim_get_current_buf() 
		local item = rawget(self, bufnr)
		if item then
			item[k] = v
		else
			item = { [k] = v }
		end
		rawset(self, bufnr, item)
	end
}), }

local function lspconfigadd(self, config)
	vim.validate({_ = {config, 't'}})
	return setmetatable(vim.tbl_deep_extend('force', require('debdut.lsp.callbacks'), self, config), {__add = lspconfigadd})
end

lsp.b.config = setmetatable({}, { __add = lspconfigadd })

