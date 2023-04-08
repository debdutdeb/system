require("neoconfig.options")
require("neoconfig.plugins")
require("neoconfig.cmp")
require("neoconfig.lsp")
require("neoconfig.dap")
require("neoconfig.vscode")
if vim.g.vscode ~= nil then
	return
end
require("neoconfig.telescope")
require("neoconfig.treesitter")
require("neoconfig.treesitter_context")
-- require("neoconfig.impatient")
require("neoconfig.autocommands")
-- require("neoconfig.nightfox")
-- require("neoconfig.kanagawa")

require("neoconfig.distant")
require("neoconfig.tokyonight")
-- require("neoconfig.gruvbox")

-- require("neoconfig.load_local_config")

require("neoconfig.neovide")
require("neoconfig.luasnip")
require("neoconfig.comments")

require("neoconfig.commands")

require("neoconfig.oil")
