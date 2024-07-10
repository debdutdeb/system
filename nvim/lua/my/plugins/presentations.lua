return {
	'sotte/presenting.nvim',
	config = function()
		local Presenting = require('presenting')

		local opts = {
			options = {
				-- The width of the slide buffer.
				width = math.ceil(vim.api.nvim_win_get_width(0) * .75),
			},
			separator = {
				-- Separators for different filetypes.
				-- You can add your own or oberwrite existing ones.
				-- Note: separators are lua patterns, not regexes.
				markdown = "^#+ ",
				org = "^*+ ",
				adoc = "^==+ ",
				asciidoctor = "^==+ ",
			},
			keymaps = {
				["n"] = function() Presenting.next() end,
				["p"] = function() Presenting.prev() end,
				["q"] = function() Presenting.quit() end,
				["f"] = function() Presenting.first() end,
				["l"] = function() Presenting.last() end,
				["<CR>"] = function() Presenting.next() end,
				["<BS>"] = function() Presenting.prev() end,
			},
		}

		Presenting.setup(opts)
	end,
	cmd = { "Presenting" },
}
