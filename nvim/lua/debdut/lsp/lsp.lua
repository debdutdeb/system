-- https://github.com/neovim/neovim/issues/12544#issuecomment-1116794687

local LSP_Handler = {
	events = {
	},
}

function LSP_Handler:emit_change(kind, option, opts)
	local fns = self.events[kind .. "_changed_" .. option] or {}

	for _, fn in ipairs(fns) do
		fn(opts)
	end
end

function LSP_Handler:on_change(kind, option, fn)
	local fns = self.events[kind .. "_changed_" .. option] or {}

	table.insert(fns, fn)

	self.events[kind .. "_changed_" .. option] = fns
end

local LSP = {
	s = {
		kind = "state"
	},
	o = {
		kind = "option"
	},
}

function LSP.s:changed(event, opts)
	LSP_Handler:emit_change(self.kind, event, opts)
end

function LSP.o:changed(event, value)
	LSP_Handler:emit_change(self.kind, event, value)
end

LSP.s = setmetatable(LSP.s, {
	__index = function(self, access)
		vim.validate({ _ = { access, { 's', 'number' } } })
		if type(access) == 'number' then
			local bufnr = access
			if bufnr < 0 then
				bufnr = #vim.tbl_keys(self) + bufnr + 1 -- negative indexing
			end

			return rawget(self, bufnr)
		end

		local bufnr = vim.api.nvim_get_current_buf()
		local tbl_for_bufnr = rawget(self, bufnr)
		return tbl_for_bufnr and tbl_for_bufnr[access]
	end,

	__newindex = function(self, key, value)
		vim.validate({ _ = { key, { 's', 'n' } } })
		if type(key) == "number" then
			rawset(self, key, value)
			return
		end

		local bufnr = vim.api.nvim_get_current_buf()
		local item = rawget(self, bufnr)
		if item then
			item[key] = value
		else
			item = { [key] = value }
		end
		rawset(self, bufnr, item)


		self:changed(key, {
			bufnr = bufnr,
			value = value,
		})
	end,
})


LSP.o = setmetatable(LSP.o, getmetatable(LSP.s))

LSP_Handler:on_change("option", "server_name", function(data)
	local server_name = data.value

	local configs = Require("lspconfig.configs")

	if configs[server_name] then
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

	Require('lspconfig')[server_name].setup(config)
end)

LSP_Handler:on_change("option", "server_name", function(data)
	-- https://github.com/neovim/nvim-lspconfig/blob/a27179f56c6f98a4cdcc79ee2971b514815a4940/lua/lspconfig/async.lua#L15C9-L15C41
	coroutine.resume(coroutine.create(function()
		local server_name = data.value

		local server = Require("lspconfig.configs")[server_name]

		-- TODO: understand and effectively port lspconfig's code here

		local bufname = vim.api.nvim_buf_get_name(data.bufnr)

		local root_dir = server.get_root_dir(Require("lspconfig.util").path.sanitize(bufname)) or
			Require("lspconfig.util").path.dirname(Require("lspconfig.util").path.sanitize(bufname))


		local new_config = server.make_config(root_dir, data.bufnr)
		if LSP.s[data.bufnr] then
			LSP.s[data.bufnr].config = new_config
		else
			LSP.s[data.bufnr] = { config = new_config }
		end
	end))
end)

function LSP.status()
	if not LSP.o.server_name then
		return ""
	end

	local language_client_str = LSP.o.server_name .. (LSP.s.client and "(connected)" or "(disconnected)")

	local clients = vim.lsp.get_clients()

	if #clients == 0 then
		return language_client_str
	end

	local filtered_client_str = vim.iter(clients):map(function(client) return client.name end):filter(function(name)
		return name ~= LSP.o
			.server_name
	end):join(",")

	if filtered_client_str == "" then
		return language_client_str
	end

	return language_client_str ..
		"::" .. filtered_client_str
end

LSP_Handler:on_change("state", "client", function(opts)
	local client_id = opts.value
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

vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(event)
		LSP.s.client = { id = event.data.client_id }
	end,
	group = vim.api.nvim_create_augroup("LSP_Handler-group", { clear = true }),
})

return { LSP = LSP, LSP_Handler = LSP_Handler }
