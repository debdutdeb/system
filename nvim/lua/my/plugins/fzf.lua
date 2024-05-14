return {
	"debdutdeb/nvim-fzf",
	keys = {
		"<leader>f",
	},
	cond = function()
		local ok, _ = pcall(require, "telescope")
		return not ok
	end,
}
