local ok, dapgo = pcall(require, "dap-go")
if not ok then
	return
end
dapgo.setup()
