local m = {}

function m.on_attach(client, bufnr)
	if client.server_capabilities.inlayHintProvider ~= nil and client.server_capabilities.inlayHintProvider then
		-- :h vim.lsp.inlay_hint
		vim.lsp.inlay_hint.enable(bufnr, true)
	end
end

m.capabilities = vim.lsp.protocol.make_client_capabilities()

return m
