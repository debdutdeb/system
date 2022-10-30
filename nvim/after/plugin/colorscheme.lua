local colorpicker = require("neoconfig.colorscheme")
local nnoremap = require("neoconfig.keymap").nnoremap

local current_colorscheme = colorpicker.get_colorscheme()

local function set_colorscheme(color)
	vim.cmd([[
	try
	  colorscheme ]] .. color .. [[ 
	catch /^Vim\%((\a\+)\)\=:E185/
	  colorscheme default
	  set background=dark
	endtry
	]])
end

set_colorscheme(current_colorscheme)

-- palenight
vim.g.palenight_terminal_italics = 1

