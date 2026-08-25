---@param parts table<string>
---@param char string
local function join(parts, char)
	local com = ""
	for _, part in ipairs(parts) do
		com = com .. char .. part
	end
	return com:gsub(".", "", 1)
end

---@enum CompatModules
local CompatModules = {
	V1 = "v1",
}

_G.compat = {
	_version = vim.version(),
	_prefix = "userspace",
	---@alias CompatTable table<string, CompatTableInner>

	---@alias CompatTableInner table<string, CompatModules>

	---@type CompatTable
	_compat_table = {
		["14"] = {
			["12"] = CompatModules.V1,
		},
	},

	modpart = function(self)
		return self._compat_table[tostring(self._version.api_level)][tostring(self._version.minor)]
	end,

	modpath = function(self, lastpart)
		return join({ self._prefix, self:modpart(), lastpart }, ".")
	end,

	---@return boolean
	check = function()
		local version = vim.version()
		if not version then
			vim.notify("could not detect neovim version, skipping loading plugins", 1)
			return false
		end
		if version.api_level ~= 14 or version.minor ~= 12 then
			vim.notify("api_level 14 is required for the current config to work, working commit is 70958dae75efc797fe85b29df11fb2ea2ebf9401, debdutdeb/neovim fork, `git clone https://github.com/debdutdeb/neovim && cd neovim && make && ls build/bin`", 1)
			return false
		end
		return true
	end,

	require = function(self, mod)
		return require(self:modpath(mod))
	end,
}
