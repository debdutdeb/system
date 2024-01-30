local ok, dap_js = pcall(require, "dap-vscode-js")
if not ok then
	return
end

dap_js.setup({
	adapters = { "pwa-node", "node-terminal" },
})

for _, language in ipairs({ "typescript", "javascript", "node" }) do
	Require("dap").configurations[language] = {
		{
			type = "pwa-node",
			request = "attach",
			name = "Attach",
			processId = Require("dap.utils").pick_process,
			cwd = "${workspaceFolder}",
		},
		{
			type = "pwa-node",
			request = "launch",
			name = "(node) start current file",
			program = "${workspaceFolder}/index.js",
		},
	}
end
