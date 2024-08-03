local harpoon = require('harpoon')

harpoon:setup()

local nnoremap = require("chaos.keymaps").nnoremap

nnoremap("<leader>a", function()
	harpoon:list():add()
end)
nnoremap("<leader>h", function()
	harpoon.ui:toggle_quick_menu(harpoon:list())
end)
nnoremap("<C-h>", function()
	harpoon:list():select(1)
end)
nnoremap("<C-j>", function()
	harpoon:list():select(2)
end)
nnoremap("<C-k>", function()
	harpoon:list():select(3)
end)
nnoremap("<C-l>", function()
	harpoon:list():select(4)
end)
nnoremap("<C-n>", function() harpoon:list():next() end);
nnoremap("<C-p>", function() harpoon:list():prev() end);
