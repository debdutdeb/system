local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
	return
end

require("debdut.lsp.configs")
require("debdut.lsp.handlers").setup()
