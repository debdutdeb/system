return {
	{
		"microsoft/vscode-js-debug",
		-- add NODE_VERSION specifically since for some reason novm is unable to install nested versions correctly, not that it should honestly
		-- iirc it adds that anyway for nested calls. But anyway. Something to fix for later
		build = "NODE_VERSION=18 npm install --legacy-peer-deps && npx gulp vsDebugServerBundle && mv dist out",
		ft = { "javascript", "typescript", "javascriptreact", "typescriptreact", "tsx", "jsx" },
	},
	{
		"mxsdev/nvim-dap-vscode-js",
		name = "dap-vscode-js",
		ft = { "javascript", "typescript", "javascriptreact", "typescriptreact", "tsx", "jsx" },
		dependencies = { "microsoft/vscode-js-debug", "mfussenegger/nvim-dap" },
		config = function(plugin)
			require("dap-vscode-js").setup({
				debugger_path = vim.fn.stdpath("data") .. "/lazy/vscode-js-debug",
				adapters = { 'chrome', 'pwa-node', 'pwa-chrome', 'pwa-msedge', 'node-terminal', 'pwa-extensionHost', 'node', 'chrome' },
			})

			for _, language in ipairs(plugin.ft) do
				require("dap").configurations[language] = {
					{
						type = "pwa-node",
						request = "attach",
						name = "Attach",
						skipFiles = {
							"<node_internals>/**",
						},
						port = 9229,
					}
				}
			end
		end,
	}
}
