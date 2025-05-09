return {
	"folke/persistence.nvim",
	enabled = vim.g.vscode == nil,
	opts = {
		-- hack to support branches kinda
		dir = vim.fn.expand(vim.fn.stdpath("state") .. "/sessions/") .. (function()
			-- add branch info
			-- multiple projects can have same branch names, but same global filename? nope
			local seperator = "/"
			if vim.fn.has("win32") == 1 then
				seperator = "[\\:]"
			end
			local origin = vim.trim(vim.fn.system("git remote get-url origin")):gsub(seperator, "%%")
			if vim.v.shell_error ~= 0 then
				-- not a git repo
				return ""
			end

			local branch = vim.trim(vim.fn.system("git branch --show-current"))
			if vim.v.shell_error ~= 0 then
				-- something wrong
				return origin .. "/"
			end

			return origin .. "%%" .. branch .. "/"
		end)(),
		options = { "buffers", "curdir", "tabpages", "winsize" },
		pre_save = nil,
		save_empty = false,
	},
	lazy = false,
}
