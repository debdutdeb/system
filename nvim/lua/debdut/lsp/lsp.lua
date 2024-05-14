-- https://github.com/neovim/neovim/issues/12544#issuecomment-1116794687

local LSP_Handler = {
	events = {
	},
}

function LSP_Handler:option_changed(option, value)
	if type(self.events["option_changed_"..option]) == "function" then
		self.events["option_changed_"..option](value)
	end
end

function LSP_Handler:on_option_change(option, fn)
	self.events["option_changed_"..option] = fn
end

local LSP = {}

LSP.b = { type = "configurations" }

function LSP.b:emit(event, value)
	-- noop
end

LSP.b = setmetatable(LSP.b, {
	__index = function(self, k)
		vim.validate({ _ = { k, 's' } })
		local bufnr = vim.api.nvim_get_current_buf()
		local tbl_for_bufnr = rawget(self, bufnr)
		return tbl_for_bufnr and tbl_for_bufnr[k]
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

		self:emit(k, v)
	end,
})

LSP.o = { type = "options" }

function LSP.o:emit(event, value)
	LSP_Handler:option_changed(event, value)
end

LSP.o = setmetatable(LSP.o, getmetatable(LSP.b))

LSP_Handler:on_option_change("server_name", function(server_name)
	lsp.b.config = Require("lspconfig.configs")[server_name]
end)

return { LSP = LSP, LSP_Handler = LSP_Handler }
