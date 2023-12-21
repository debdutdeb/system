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

local Mutate = {}

function Mutate.config(v)
	vim.validate({ cfg = { v, { 't', 'nil' } } })
	return setmetatable(v and vim.tbl_deep_extend('force', v, require('debdut.lsp.base_config')) or {}, {
		__add = function(tbl, v)
			vim.validate({ cfg = { v, 't' } })
			return setmetatable(vim.tbl_deep_extend('force', tbl, v),
				getmetatable(tbl))
		end,
	})
end

Mutate = setmetatable(Mutate, { __index = function(_, i) return function(i) return i end end, })

lsp.b = setmetatable({}, {
	__index = function(self, k)
		vim.validate({ _ = { k, 's' } })
		local bufnr = vim.api.nvim_get_current_buf()
		local tbl_for_bufnr = rawget(self, bufnr)
		return tbl_for_bufnr and tbl_for_bufnr[k] or Mutate[k](nil)
	end,

	__newindex = function(self, k, v)
		vim.validate({ _ = { k, 's' } })
		local bufnr = vim.api.nvim_get_current_buf()
		local item = rawget(self, bufnr)
		if item then
			item[k] = Mutate[k](v)
		else
			item = { [k] = Mutate[k](v) }
		end
		rawset(self, bufnr, item)
	end
})
