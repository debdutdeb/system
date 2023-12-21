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

_G.lsp = {}

lsp.b = {
	[0] = {
		config = setmetatable(require('debdut.lsp.base_config'), {
			__add = function(tbl, v)
				vim.validate({ cfg = { v, 't' } })
				return setmetatable(vim.tbl_deep_extend('force', tbl, v),
					getmetatable(tbl))
			end,
		}),
	}
}

lsp.b = setmetatable(lsp.b, {
	__index = function(self, k)
		vim.validate({ _ = { k, 's' } })
		local bufnr = vim.api.nvim_get_current_buf()
		local tbl_for_bufnr = rawget(self, bufnr)
		return tbl_for_bufnr and tbl_for_bufnr[k] or self[0][k]
	end,

	__newindex = function(self, k, v)
		vim.validate({ _ = { k, 's' } })
		local bufnr = vim.api.nvim_get_current_buf()
		local item = rawget(self, bufnr)
		if item then
			item[k] = v
		else
			item = { [k] = v }
		end
		rawset(self, bufnr, item)
	end
})
