-- https://github.com/neovim/neovim/issues/12544#issuecomment-1116794687

local LSP_Handler = {
	events = {
	},
}

function LSP_Handler:emit_change(kind, option, value)
	if type(self.events[kind .. "_changed_" .. option]) == "function" then
		self.events[kind .. "_changed_" .. option](value)
	end
end

function LSP_Handler:on_change(kind, option, fn)
	self.events[kind .. "_changed_" .. option] = fn
end

local LSP = {}

LSP.s = { type = "state" }

function LSP.s:changed(event, value)
	LSP_Handler:emit_change(self.type, event, value)
end

LSP_Handler:on_change("state", "client", function(client_id)
	if client_id == nil then
		-- connection just dropped
		vim.iter(vim.api.nvim_list_bufs()):each(function(bufnr)
			local state = rawget(LSP.s, bufnr)
			if state then
				state.client = nil
			end
		end)
	end
end)

LSP.s = setmetatable(LSP.s, {
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

		self:changed(k, v)
	end,
})

LSP.o = { type = "option" }

function LSP.o:changed(event, value)
	LSP_Handler:emit_change(self.type, event, value)
end

LSP.o = setmetatable(LSP.o, getmetatable(LSP.s))

LSP_Handler:on_change("option", "server_name", function(server_name)
	local server = Require("lspconfig.configs")[server_name]

	-- https://github.com/neovim/nvim-lspconfig/blob/a27179f56c6f98a4cdcc79ee2971b514815a4940/lua/lspconfig/async.lua#L15C9-L15C41
	coroutine.resume(coroutine.create(function()
		lsp.s.config = server.make_config(server.get_root_dir(Require('lspconfig.util').path.sanitize(vim.api
		.nvim_buf_get_name(0))))
	end))
end)

function LSP.status()
	if not lsp.o.server_name then
		return ""
	end

	local language_client_str = lsp.o.server_name .. (lsp.s.client and "(connected)" or "(disconnected)")

	local clients = vim.lsp.get_clients()

	if #clients == 0 then
		return language_client_str
	end

	local filtered_client_str = vim.iter(clients):map(function(client) return client.name end):filter(function(name)
		return name ~= lsp.o
			.server_name
	end):join(",")

	if filtered_client_str == "" then
		return language_client_str
	end

	return language_client_str ..
		"::" .. filtered_client_str
end

return { LSP = LSP, LSP_Handler = LSP_Handler }
