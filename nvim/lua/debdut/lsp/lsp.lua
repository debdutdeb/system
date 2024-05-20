-- https://github.com/neovim/neovim/issues/12544#issuecomment-1116794687

local LSP_Handler = {
	events = {
	},
}

function LSP_Handler:emit_change(kind, option, value)
	local fns = self.events[kind .. "_changed_" .. option] or {}
	for _, fn in ipairs(fns) do
		fn(value)
	end
end

function LSP_Handler:on_change(kind, option, fn)
	local fns = self.events[kind .. "_changed_" .. option] or {}

	table.insert(fns, fn)

	self.events[kind .. "_changed_" .. option] = fns
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
	local lspconfig = Require("lspconfig")

	if lspconfig[server_name] then
		return
	end

	local ok, config = pcall(require, "debdut.lsp.settings." .. server_name)
	if not ok then config = {} end

	config.autostart = false
	config.on_attach = function(client, bufnr)
		if client.server_capabilities.inlayHintProvider ~= nil and client.server_capabilities.inlayHintProvider then
			-- :h vim.lsp.inlay_hint
			vim.lsp.inlay_hint.enable(bufnr, true)
		end
	end

	lspconfig.setup(config)
end)

LSP_Handler:on_change("option", "server_name", function(server_name)
	-- https://github.com/neovim/nvim-lspconfig/blob/a27179f56c6f98a4cdcc79ee2971b514815a4940/lua/lspconfig/async.lua#L15C9-L15C41
	coroutine.resume(coroutine.create(function()
		local server = Require("lspconfig.configs")[server_name]

		-- TODO: understand and effectively port lspconfig's code here

		local bufname = vim.api.nvim_buf_get_name(0)

		local root_dir = server.get_root_dir(Require("lspconfig.util").path.sanitize(bufname)) or
			Require("lspconfig.util").path.dirname(Require("lspconfig.util").path.sanitize(bufname))

		lsp.s.config = server.make_config(root_dir)
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

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(event)
		lsp.s.client = { id = event.data.client_id }
	end,
	group = vim.api.nvim_create_augroup("LSP_Handler-group", { clear = true }),
})

return { LSP = LSP, LSP_Handler = LSP_Handler }
