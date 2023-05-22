local M = {}

local MODES = {
	c = "COMMAND",
	v = "VISUAL",
	i = "INSERT",
	V = "VISUAL BLOCK",
	n = "NORMAL",
	t = "TERM",
}

function _G.current_mode()
	local mode = vim.api.nvim_get_mode().mode
	return MODES[mode] or mode
end

function _G.lsp_progress()
	if #vim.lsp.buf_get_clients() == 0 then
		return ""
	end

	local lsp = vim.lsp.util.get_progress_messages()[1]
	if lsp then
		local name = lsp.name or ""
		local msg = lsp.message or ""
		local percentage = lsp.percentage or 0
		local title = lsp.title or ""
		return string.format(" %%<%s: %s %s (%s%%%%) ", name, title, msg, percentage)
	end
	return ""
end

function _G.dap_progress()
	local hum, dap = pcall(require, "dap")
	if not hum then
		return
	end
	return dap.status()
end

function M.get_statusline()
	return [[%{luaeval("current_mode()")}_%{luaeval("lsp_progress()")}_%{luaeval("dap_progress()")}%=r/o=%R,l=%L,c=%c,%%=%p,help=%H,preview=%W,ft=%Y%M]]
end

return M
