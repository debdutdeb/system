--[[ local status, lsp_util = pcall(require, "lspconfig.util")
if not status then
	return
end

local function load_config(file)
	local vimrc = io.open(file)
	if vimrc == nil then
		return false
	end

	local vim_script = vimrc:read("*all")
	vimrc:close()
	return vim_script
end

print(vim.inspect(vim.lsp.list_workspace_folders()))
assert(false)
local loc_configs = { ".exrc", ".vimrc", ".nvimrc" }

for _, file in pairs(loc_configs) do
	local path = lsp_util.root_pattern(file)
	if type(path) == "string" then
		vim.cmd(load_config(file))
		return
	end
end

local curr_dir = lsp_util.path.dirname(vim.fn.expand("%:p"))
if not curr_dir then
	-- no file open yet
	return
end
for _, file in pairs(loc_configs) do
	local vim_script = load_config(curr_dir .. "/" .. file)
	if vim_script then
		vim.cmd(vim_script)
		return
	end
end ]]
