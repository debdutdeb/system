local d_ok, distant = pcall(require, "distant")
local ds_ok, distant_settings = pcall(require, "distant.settings")
if not d_ok or not ds_ok then
	return
end

distant.setup({
	-- Applies Chip's personal settings to every machine you connect to
	--
	-- 1. Ensures that distant servers terminate with no connections
	-- 2. Provides navigation bindings for remote directories
	-- 3. Provides keybinding to jump into a remote file's parent directory
	["*"] = distant_settings.chip_default(),
})
