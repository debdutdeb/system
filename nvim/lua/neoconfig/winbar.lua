local M = {}

local MODES = {
	c = "COMMAND",
	v = "VISUAL",
	i = "INSERT",
	X = "VISUAL BLOCK",
	n = "NORMAL",
	t = "TERM",
}

function M.current_mode()
	return MODES[vim.api.nvim_get_mode().mode]
end

return M
