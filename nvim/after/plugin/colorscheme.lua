local colorpicker = require("debdut.colorscheme")

local current_colorscheme = colorpicker.get_colorscheme()

vim.cmd([[colorscheme ]] .. current_colorscheme)

-- palenight
vim.g.palenight_terminal_italics = 1
