if vim.g.vscode == nil then return end

local nnoremap = require("neoconfig.keymap").nnoremap

nnoremap("zff", function() vim.fn.VSCodeNotify("editor.toggleFold") end)
