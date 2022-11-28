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
