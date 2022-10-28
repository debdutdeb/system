local ok, pickers = pcall(require, "telescope.pickers")
if not ok then
	return
end

-- if telescope picker is there so should all of the following
-- TODO still add safe blocks
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")

-- TODO add more nightfox colorschemes
local colors = { "tokyonight", "nightfox", "palenight" }

local M = {}

function M.pick_colorscheme(cb)
	pickers
		.new({ border = false }, {
			prompt_title = "Colorschemes",
			finder = finders.new_table({ results = colors }),
			sorter = conf.generic_sorter({ border = false }),
			attach_mappings = function(prompt_bufnr)
				actions.select_default:replace(function()
					actions.close(prompt_bufnr)
					cb(action_state.get_selected_entry()[1])
				end)
				return true
			end,
		})
		:find()
end

function M.save_colorscheme(colorscheme)
	if colorscheme == "" then
		-- TODO add a warning message like vim.notify
		return
	end
	local file = io.open("/Users/debdut/.config/nvim/after/plugin/.colorscheme", "w")
	if file == nil then
		-- TODO notify about the error
		return
	end
	print(colorscheme)
	file:write(colorscheme)
	io.close(file)
end

function M.get_colorscheme()
	local file = io.open("/Users/debdut/.config/nvim/after/plugin/.colorscheme", "r")
	vim.notify(vim.inspect(file))
	if file == nil then
		-- TODO notify about the error
		return ""
	end
	local colorscheme = file:read("l")
	file:close()
	return colorscheme
end

return M
