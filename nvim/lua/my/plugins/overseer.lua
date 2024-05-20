return {
	'stevearc/overseer.nvim',
	opts = {
		dap = false,
	},
	cmd = function(plugin, cmd)
		local cmds = {}

		for _, suffix in ipairs({ "Run", "Toggle", "Info", "Build" }) do
			table.insert(cmds, "Overseer" .. suffix)
		end

		return cmds
	end,
}
