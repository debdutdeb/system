-- local Remap = require("chaos.keymaps")
-- local nnoremap = Remap.nnoremap

local vscode = require 'vscode'

---@class Callback
---@field callback string | function
---@field fallback string | function

local function bind(op, outer_opts)
	outer_opts = outer_opts or { noremap = true, silent = true }

	---@param foo string|function
	local run = function(foo)
		if type(foo) == "string" then
			return vim.api.nvim_command_output(foo)
		else
			return foo()
		end
	end

	---@param lhs string
	---@param rhs string | function
	---@param opts table | nil
	return function(lhs, rhs, opts)
		local func = rhs
		if type(rhs) == "table" then
			func = function()
				local ok, result = pcall(run, rhs.callback)
				if not ok then
					vim.notify("falling back since primary failed " .. result)
					run(rhs.fallback)
				end
			end
		end

		opts = vim.tbl_extend("force", outer_opts, opts or {})
		vim.keymap.set(op, lhs, func or function() end, opts)
	end
end

local nmap = bind("n", { noremap = false })
local nnoremap = bind("n")
local vnoremap = bind("v")
local xnoremap = bind("x")
local inoremap = bind("i")

local function bind_send_keys(mode)
	return function(key)
		vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, false)
	end
end

local nsend_keys = bind_send_keys("n")
local vsend_keys = bind_send_keys("v")
local xsend_keys = bind_send_keys("x")
local isend_keys = bind_send_keys("i")
local tsend_keys = bind_send_keys("t")

vim.keymap.set("n", "<leader>f", function()
	-- vscode.action("workbench.action.toggleSidebarVisibility")
	vscode.action("workbench.action.quickOpen")
end, { desc = "Open sidebar" })

vim.keymap.set("n", "<leader>c", function()
	vscode.action("workbench.panel.chat")
end, { desc = "Toggle copilot chat" })

nnoremap("<leader>e", function()
	vscode.action("workbench.action.toggleSidebarVisibility")
end);

-- LSP
nnoremap("<leader>lr", function()
	vscode.action("editor.action.rename")
end)

nnoremap("<leader>ls", function()
	vscode.action("workbench.action.gotoSymbol")
end)

nnoremap("<leader>lS", function()
	vscode.action("workbench.action.showAllSymbols")
end)

nnoremap("<leader>lf", function()
	vscode.action("editor.action.formatDocument")
end)

nnoremap("<leader>lF", function()
	vscode.action("editor.action.formatSelection")
end)

nnoremap("<leader>lR", function()
	vscode.action("editor.action.goToReferences")
end)

nnoremap("<leader>lRR", function()
	vscode.action("references-view.findReferences")
end)

nnoremap("<leader>lRR", function()
	vscode.action("references-view.findReferences")
end)

-- misc
-- For wraps
nnoremap("j", 'v:count == 0 ? "gj" : "j"', { expr = true, silent = true })
nnoremap("k", 'v:count == 0 ? "gk" : "k"', { expr = true, silent = true })

-- Move text up and down
-- normal mode
nnoremap("<A-J>", ":m .+1<CR>==")
nnoremap("<A-K>", ":m .-2<CR>==")
-- insert mode
inoremap("<A-j>", "<Esc>:m .+1<CR>==gi")
inoremap("<A-k>", "<Esc>:m .-2<CR>==gi")

inoremap("<A-J>", "<Esc>:m .+1<CR>==gi")
inoremap("<A-K>", "<Esc>:m .-2<CR>==gi")
-- visual mode
vnoremap("<A-j>", ":m .+1<CR>==")
vnoremap("<A-k>", ":m .-2<CR>==")

vnoremap("<A-J>", ":m .+1<CR>==")
vnoremap("<A-K>", ":m .-2<CR>==")

-- Insert --
-- Press jk fast to exit insert mode
inoremap("jk", "<ESC>")
-- Press kj fast to exit insert mode
inoremap("kj", "<ESC>")

-- Visual --
-- Stay in indent mode
vnoremap("<", "<gv")
vnoremap(">", ">gv")

-- don't save overwritten text on register
vnoremap("p", '"_dP')
nnoremap("<A-p>", "o<Esc>p")
nnoremap("<A-P>", "O<Esc>jP")
inoremap("<A-p>", "<Return><C-r>\"")
inoremap("<A-P>", "<Esc>O<Esc>jPi")

-- find and replace
vnoremap("<leader>r", "\"hy:%s/<C-r>h//g<left><left>", { silent = false })
xnoremap("<leader>r", "\"hy:%s/<C-r>h//g<left><left>", { silent = false })

-- Visual Block --
-- Move text up and down
xnoremap("J", ":move '>+1<CR>gv-gv")
xnoremap("K", ":move '<-2<CR>gv-gv")
xnoremap("<A-j>", ":move '>+1<CR>gv-gv")
xnoremap("<A-k>", ":move '<-2<CR>gv-gv")

xnoremap("<A-J>", ":move '>+1<CR>gv-gv")
xnoremap("<A-K>", ":move '<-2<CR>gv-gv")


nnoremap("<M-o>", "o<Esc>")
nnoremap("<M-O>", "O<Esc>")

-- nnoremap("<leader>f", function()
-- 	vscode.runCommand("editor.action.")
-- end)
--
