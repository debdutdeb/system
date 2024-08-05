local harpoon = require('harpoon')

harpoon:setup()

local nnoremap = require("chaos.keymaps").nnoremap

nnoremap("<leader>a", function()
	harpoon:list():add()
end)
nnoremap("<leader>h", function()
	harpoon.ui:toggle_quick_menu(harpoon:list())
end)
nnoremap("<A-1>", function()
	harpoon:list():select(1)
end)
nnoremap("<A-2>", function()
	harpoon:list():select(2)
end)
nnoremap("<A-3>", function()
	harpoon:list():select(3)
end)
nnoremap("<A-4>", function()
	harpoon:list():select(4)
end)
nnoremap("<C-n>", function() harpoon:list():next() end);
nnoremap("<C-p>", function() harpoon:list():prev() end);
