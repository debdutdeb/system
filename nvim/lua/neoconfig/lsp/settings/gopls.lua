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

local ok, lspcontainers = pcall(require, "lspcontainers")
if not ok then
	return settings
end

--[[ settings.cmd = lspcontainers.command("gopls", {
	image = "debdutdeb/dind-gopls:1.19",
	cmd = function(runtime, volume, image)
		return {
			runtime,
			"container",
			"run",
			"--interactive",
			"--rm",
			"--volume",
			volume,
			"--privileged",
			image,
		}
	end,
}) ]]

-- settings.cmd = lspcontainers.command("gopls")

-- settings.cmd = {"docker", "exec", "-i", "gopls", "gopls", "-vv"}

return settings
