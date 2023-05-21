vim.cmd([[
augroup _general_settings
autocmd!
autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR> 
autocmd TextYankPost * silent!lua require('vim.highlight').on_yank({higroup = 'Visual', timeout = 200}) 
autocmd BufWinEnter * :set formatoptions-=cro
autocmd FileType qf set nobuflisted
augroup end

augroup _git
autocmd!
autocmd FileType gitcommit setlocal wrap
autocmd FileType gitcommit setlocal spell
augroup end

augroup _markdown
autocmd!
autocmd FileType markdown setlocal wrap
autocmd FileType markdown setlocal spell
augroup end

augroup _auto_resize
autocmd!
autocmd VimResized * tabdo wincmd = 
augroup end

autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif 

au BufRead,BufNewFile *.bash setfiletype bash

augroup remember_folds
autocmd!
au BufWinLeave ?* mkview 1
au BufWinEnter ?* silent! loadview 1
augroup END
]])

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
	pattern = "plugins.lua",
	group = vim.api.nvim_create_augroup("PackerUserConfig", {
		clear = false,
	}),
	callback = function(_)
		vim.cmd("so | PackerSync")
	end,
	desc = "Update all plugins if the plugins.lua is updated",
})

vim.api.nvim_create_autocmd("VimEnter", {
	callback = function()
		if vim.api.nvim_buf_get_name(0) == "" then
			local ok, telescope = pcall(require, "telescope.builtin")
			if not ok then
				return
			end
			telescope.find_files()
		end
	end,
})
