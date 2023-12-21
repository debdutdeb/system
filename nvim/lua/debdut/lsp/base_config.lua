local m = {}

function m.on_attach(client, bufnr)
	if client.server_capabilities.inlayHintProvider ~= nil and client.server_capabilities.inlayHintProvider then
		vim.lsp.inlay_hint(bufnr)
	end
end

m.capabilities = vim.lsp.protocol.make_client_capabilities()

return m
