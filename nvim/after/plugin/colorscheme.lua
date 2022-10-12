vim.cmd([[
try
  " colorscheme nightfox
  colorscheme tokyonight
catch /^Vim\%((\a\+)\)\=:E185/
  colorscheme default
  set background=dark
endtry
]])

-- just to fix the above highlight
-- thanks treesitter

-- palenight
vim.g.palenight_terminal_italics = 1

