local M = {}

-- TODO: backfill this to template
M.setup = function()
	local config = {
		-- enable virtual text
		virtual_text = true,
		update_in_insert = true,
		underline = true,
		severity_sort = true,
		float = {
			source = "if_many",
			header = "",
			prefix = "",
		},
	}

	vim.diagnostic.config(config)

	vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
		border = nil,
		width = 60,
	})

	vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
		border = nil,
		width = 60,
	})
end

local function lsp_highlight_document(client)
	-- Set autocommands conditional on server_capabilities
	local status_ok, illuminate = pcall(require, "illuminate")
	if not status_ok then
		return
	end
	illuminate.on_attach(client)
	-- end
end

local function lsp_keymaps(bufnr)
	local opts = { noremap = true, silent = true }

	local function with_callback(options, cb)
		options.callback = cb
		return options
	end

	local function set_keymap(buffer, mode, keys, options, command)
		if options.callback ~= nil then
			command = ""
		end
		vim.api.nvim_buf_set_keymap(buffer, mode, keys, command, options)
	end

	set_keymap(bufnr, "n", "gD", with_callback(opts, vim.lsp.buf.declaration))
	set_keymap(bufnr, "n", "gd", with_callback(opts, vim.lsp.buf.definition))
	set_keymap(bufnr, "n", "K", with_callback(opts, vim.lsp.buf.hover))
	set_keymap(bufnr, "n", "gi", with_callback(opts, vim.lsp.buf.implementation))
	set_keymap(bufnr, "n", "<C-k>", with_callback(opts, vim.lsp.buf.signature_help))
	-- set_keymap("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
	set_keymap(bufnr, "n", "gr", with_callback(opts, vim.lsp.buf.references))
	-- set_keymap("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
	-- set_keymap("n", "<leader>f", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
	set_keymap(bufnr, "n", "[d", with_callback(opts, vim.diagnostic.goto_prev))
	set_keymap(bufnr, "n", "gl", with_callback(opts, vim.diagnostic.open_float))
	set_keymap(bufnr, "n", "]d", with_callback(opts, vim.diagnostic.goto_next))
	set_keymap(bufnr, "n", "<leader>x", with_callback(opts, vim.diagnostic.setloclist))
	vim.cmd([[ command! Format execute 'lua vim.lsp.buf.format{async=true}' ]])
end

M.on_attach = function(client, bufnr)
	-- TODO: refactor this into a method that checks if string in list
	if client.name == "tsserver" or client.name == "clangd" or client.name == "perlnavigator" then
		client.server_capabilities.document_formatting = false
	end
	lsp_keymaps(bufnr)
	lsp_highlight_document(client)
	local status, inlay_hints = pcall(require, "lsp-inlayhints")
	if status then
		inlay_hints.on_attach(client, bufnr)
	end

	if vim.bo[bufnr].buftype ~= "" or vim.bo[bufnr].filetype == "helm" then
		vim.diagnostic.disable(bufnr)
		vim.defer_fn(function()
			vim.diagnostic.reset(nil, bufnr)
		end, 1000)
	end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()

local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not status_ok then
	return
end

M.capabilities = cmp_nvim_lsp.default_capabilities(capabilities)

return M
