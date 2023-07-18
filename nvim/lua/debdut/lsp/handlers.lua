local M = {}

M.on_attach = function(client, bufnr)
	-- TODO: refactor this into a method that checks if string in list
	if client.name == "tsserver" or client.name == "clangd" or client.name == "perlnavigator" then
		client.server_capabilities.document_formatting = false
	end
	if vim.bo[bufnr].buftype ~= "" or vim.bo[bufnr].filetype == "helm" then
		vim.diagnostic.disable(bufnr)
		vim.defer_fn(function()
			vim.diagnostic.reset(nil, bufnr)
		end, 1000)
	end
	if client.server_capabilities.inlayHintProvider ~= nil or client.server_capabilities.inlayHintProvider == true then
		vim.lsp.inlay_hint(bufnr)
	end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()

local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not status_ok then
	M.capabilities = capabilities
else
	M.capabilities = cmp_nvim_lsp.default_capabilities(capabilities)
end

return M
