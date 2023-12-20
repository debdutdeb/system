vim.cmd([[
augroup _general_settings
autocmd!
autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
autocmd TextYankPost * silent!lua require('vim.highlight').on_yank({higroup = 'Visual', timeout = 200})
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

vim.api.nvim_create_autocmd("VimLeave", {
	callback = function(_)
		if not vim.uv.fs_stat(".vim") then
			vim.fn.mkdir(".vim")
		end
		vim.cmd(":mksession! " .. vim.fn.getcwd() .. "/.vim/session")
	end,
})

vim.api.nvim_create_autocmd("VimEnter", {
	callback = function(_)
		if true then return end
		local args = vim.api.nvim_command_output ":args"
		if args then
			if string.match(args, "%[.+%]") then
				return
			end
		end
		if vim.uv.fs_stat(".vim/session") then
			vim.cmd ":source .vim/session"
		end
	end,
	nested = true,
})

--[[ vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
	pattern = "*.tf",
	callback = function()
		vim.cmd "set ft=hcl"
	end
}) ]]

local qflist_group = vim.api.nvim_create_augroup("qflist_autoopen", { clear = true })

vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	pattern = "l*", -- locationlist
	callback = function(_) vim.cmd ":lopen" end,
})

vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	pattern = "[^l]*", -- quickfixlist
	callback = function(_) vim.cmd ":copen" end,
})
