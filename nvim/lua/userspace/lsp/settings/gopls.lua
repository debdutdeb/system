local settings = {
	settings = {
		gopls = {
			hints = {
				assignVariableTypes = true,
				compositeLiteralFields = true,
				constantValues = true,
				functionTypeParameters = true,
				parameterNames = true,
				rangeVariableTypes = true,
			},
		},
	},
}

--[[ if goplsHost ~= nil and goplsPort ~= nil then
	settings.cmd = vim.lsp.rpc.connect(goplsHost, goplsPort)
end ]]

return settings
