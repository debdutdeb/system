local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
	return
end

require("neoconfig.lsp.inlay_hints")
require("neoconfig.lsp.configs")
require("neoconfig.lsp.handlers").setup()
require("neoconfig.lsp.null-ls")
