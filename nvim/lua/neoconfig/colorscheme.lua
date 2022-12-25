local M = {}

function M.save_colorscheme(colorscheme)
	if colorscheme == nil then
		vim.notify("no colorscheme passed")
		return
	end
	local file = io.open(vim.fn.expand("$HOME/.config/nvim/after/plugin/.colorscheme"), "w")
	if file == nil then
		vim.notify("failed to open colorscheme file")
		return
	end
	file:write(colorscheme)
	io.close(file)
end

function M.get_colorscheme()
	local file = io.open(vim.fn.expand("$HOME/.config/nvim/after/plugin/.colorscheme"), "r")
	if file == nil then
		vim.notify("failed to open colorscheme file")
		return ""
	end
	local colorscheme = file:read("l")
	file:close()
	return colorscheme
end

return M
