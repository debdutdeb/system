-- Thanks Primagen
-- FYI - https://github.com/ThePrimeagen/.dotfiles/blob/master/nvim/.config/nvim/lua/theprimeagen/keymap.lua

local M = {}

local function bind(op, outer_opts)
	outer_opts = outer_opts or { noremap = true, silent = true }
	return function(lhs, rhs, opts)
		opts = vim.tbl_extend("force", outer_opts, opts or {})
		vim.keymap.set(op, lhs, rhs or function() end, opts)
	end
end

M.nmap = bind("n", { noremap = false })
M.nnoremap = bind("n")
M.vnoremap = bind("v")
M.xnoremap = bind("x")
M.inoremap = bind("i")
M.Vnoremap = bind("V")

local function bind_send_keys(mode)
	return function(key)
		vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, false)
	end
end

M.nsend_keys = bind_send_keys("n")
M.vsend_keys = bind_send_keys("v")
M.xsend_keys = bind_send_keys("x")
M.isend_keys = bind_send_keys("i")

return M
