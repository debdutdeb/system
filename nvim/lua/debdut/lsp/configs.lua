local lspconfig = require("lspconfig")

local servers = require("debdut.lsp.servers")

table.insert(servers, "helm_ls")

local lsp_handlers = require("debdut.lsp.handlers")

for _, server in pairs(servers) do
	local opts = {
		on_attach = lsp_handlers.on_attach,
		capabilities = lsp_handlers.capabilities,
		autostart = true,
	}
	local has_custom_opts, server_custom_opts = pcall(require, "neoconfig.lsp.settings." .. server)
	if has_custom_opts then
		opts = vim.tbl_deep_extend("force", opts, server_custom_opts)
	end
	lspconfig[server].setup(opts)
end
