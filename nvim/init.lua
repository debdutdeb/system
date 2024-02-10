function _G.Require(mod)
	if package.loaded[mod] then return package.loaded[mod] end
	local ok, p = pcall(require, mod)
	if not ok then
		vim.notify("module not found " .. mod)
		return nil
	end

	return p
end

vim.loader.enable()

require("debdut.options")
require("debdut.plugins")
require("debdut.commands")
require('debdut.lsp')

