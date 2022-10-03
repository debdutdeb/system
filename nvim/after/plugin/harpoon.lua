local harpoon_mark_ok, harpoon_mark = pcall(require, "harpoon.mark")
local harpoon_ui_ok, harpoon_ui = pcall(require, "harpoon.ui")
if not (harpoon_mark_ok and harpoon_ui_ok) then
	return
end

local nnoremap = require("neoconfig.keymap").nnoremap

nnoremap("<leader>a", function()
	harpoon_mark.add_file()
end)
nnoremap("<leader>h", function()
	harpoon_ui.toggle_quick_menu()
end)
nnoremap("<A-1>", function()
	harpoon_ui.nav_file(1)
end)
nnoremap("<A-2>", function()
	harpoon_ui.nav_file(2)
end)
nnoremap("<A-3>", function()
	harpoon_ui.nav_file(3)
end)
nnoremap("<A-4>", function()
	harpoon_ui.nav_file(4)
end)
