local status_ok, nightfox = pcall(require, "nightfox")
if not status_ok then
	return
end

nightfox.setup({
	options = {
		compile_path = vim.fn.stdpath("cache") .. "/nightfox",
		compile_file_suffix = "_compiled",
		transparent = false,
		terminal_colors = true,
		dim_inactive = false,
		styles = {
			comments = "italic",
			conditionals = "italic",
			constants = "NONE",
			functions = "italic,underline",
			keywords = "NONE",
			numbers = "NONE",
			operators = "NONE",
			strings = "NONE",
			types = "italic,bold",
			variables = "NONE",
		},
		inverse = {
			match_paren = false,
			visual = false,
			search = false,
		},
	},
	palettes = {},
	specs = {},
	groups = {},
})
