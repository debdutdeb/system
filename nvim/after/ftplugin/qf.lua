local remap = require('debdut.keymap')
remap.nnoremap("o", function()
	-- FIXME i don't know why feed_keys does not trigger the next keymap
	-- initially pressing 'o' closed the split but then it didn't once
	-- i changed to feed_keys
	-- Likelyhood of this breaking is high
	remap.nsend_keys "<CR><C-w>p"
end)
remap.nnoremap("<cr>", function()
	local w = vim.api.nvim_get_current_win()
	vim.cmd "normal! <CR>"
	if vim.api.nvim_win_is_valid(w) then vim.api.nvim_win_close(w, true) end
end)

vim.b.debdut='qf'
