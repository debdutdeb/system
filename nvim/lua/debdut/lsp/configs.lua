local lspconfig = require("lspconfig")

local servers = require("debdut.lsp.servers")

table.insert(servers, "helm_ls")

local lsp_handlers = require("debdut.lsp.handlers")

local function get_config(server)
	local opts = {
		on_attach = lsp_handlers.on_attach,
		capabilities = lsp_handlers.capabilities,
		autostart = false,
	}
	local has_custom_opts, server_custom_opts = pcall(require, "debdut.lsp.settings." .. server)
	if has_custom_opts then
		opts = vim.tbl_deep_extend("force", opts, server_custom_opts)
	end
	return opts
end

for _, server in pairs(servers) do
	local opts = get_config(server)
	lspconfig[server].setup(opts)
end

return {
	get_config = get_config,
}
