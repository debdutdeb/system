return {
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"nvim-neotest/nvim-nio",
			'stevearc/overseer.nvim',
		},
		keys = { "<leader>db", "<leader>dc" },
		-- cmd = { "DebugWithArgs" },
		config = function()
			require("debdut.dap")
		end,
	},
	{
		"rcarriga/nvim-dap-ui",
		dependencies = {
			"mfussenegger/nvim-dap",
			"theHamsta/nvim-dap-virtual-text",
		}
	}
}
