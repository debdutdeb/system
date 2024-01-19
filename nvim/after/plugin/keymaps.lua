local Remap = require("debdut.keymap")

local nnoremap = Remap.nnoremap
local inoremap = Remap.inoremap
local vnoremap = Remap.vnoremap
local xnoremap = Remap.xnoremap

local tsend_keys = Remap.tsend_keys

-- For wraps
nnoremap("j", 'v:count == 0 ? "gj" : "j"', { expr = true, silent = true })
nnoremap("k", 'v:count == 0 ? "gk" : "k"', { expr = true, silent = true })

-- Navigate buffers
nnoremap("<S-l>", ":bnext<CR>")
nnoremap("<S-h>", ":bprevious<CR>")

-- Move text up and down
nnoremap("<A-j>", "<Esc>:m .+1<CR>==gi")
nnoremap("<A-k>", "<Esc>:m .-2<CR>==gi")

-- Insert --
-- Press jk fast to exit insert mode
inoremap("jk", "<ESC>")
-- Press kj fast to exit insert mode
inoremap("kj", "<ESC>")

-- Visual --
-- Stay in indent mode
vnoremap("<", "<gv")
vnoremap(">", ">gv")

-- Move text up and down
vnoremap("<A-j>", ":m .+1<CR>==")
vnoremap("<A-k>", ":m .-2<CR>==")
vnoremap("p", '"_dP')

-- find and replace
vnoremap("<leader>r", "\"hy:%s/<C-r>h//g<left><left>", { silent = false })
xnoremap("<leader>r", "\"hy:%s/<C-r>h//g<left><left>", { silent = false })

-- Visual Block --
-- Move text up and down
xnoremap("J", ":move '>+1<CR>gv-gv")
xnoremap("K", ":move '<-2<CR>gv-gv")
xnoremap("<A-j>", ":move '>+1<CR>gv-gv")
xnoremap("<A-k>", ":move '<-2<CR>gv-gv")

local function leadernnoremap(key, cmd, opts)
	nnoremap("<leader>" .. key, cmd, opts)
end

-- LSP
leadernnoremap("ll", "<cmd>LspStart<cr>")
leadernnoremap("lL", "<cmd>LspStartWithAutocomplete<cr>")
leadernnoremap("la", vim.lsp.buf.code_action)
leadernnoremap("ld", vim.diagnostic.setqflist)
leadernnoremap("lf", function()
	vim.lsp.buf.format({ async = true })
end)
leadernnoremap("lj", vim.lsp.diagnostic.goto_prev)
leadernnoremap("lk", vim.lsp.diagnostic.goto_next)
leadernnoremap("lc", vim.lsp.codelens.run)
leadernnoremap("lr", vim.lsp.buf.rename)
leadernnoremap("ls", vim.lsp.buf.document_symbol)
leadernnoremap("lS", vim.lsp.buf.workspace_symbol)
nnoremap("lR", vim.lsp.buf.references)
nnoremap("lI", vim.lsp.buf.implementation)

leadernnoremap("g", function()
	local files = {}
	for filename in string.gmatch(vim.system({ "git", "ls-files" }):wait().stdout, "([^\n]+)") do
		table.insert(files, { filename = filename, lnum = 1 })
	end
	vim.fn.setloclist(0, files)
end)

leadernnoremap("b", function()
	tsend_keys(":ls<cr>:b ")
end, { silent = false })

leadernnoremap("w", "<cmd>w!<cr>")
leadernnoremap("q", "<cmd>q!<cr>")
leadernnoremap("c", "<cmd>bd!<cr>")
leadernnoremap("f", function()
	local co = coroutine.create(function()
		local choices = require("fzf").fzf("fd", "--multi", {
			relative = "cursor",
			border   = false
		})

		if choices == nil then return end

		if #choices == 1 then
			vim.cmd(":e " .. choices[1])
			return
		end

		local fnames = {}
		for _, name in pairs(choices) do
			table.insert(fnames, { filename = name, lnum = 1 })
		end

		vim.fn.setloclist(0, fnames)
	end)

	coroutine.resume(co)
end)
leadernnoremap("F", function()
	tsend_keys(":silent lgrep! ")
end, { silent = false })
leadernnoremap("e", "<cmd>Explore<cr>") -- open netrw


leadernnoremap("/", "<cmd>nohl<cr>")

-- often I have to delete the entire block including the starting line
-- leadernnoremap("dd", "da{dd")

leadernnoremap("ka", "<cmd>Kapply<cr>")

-- lsp keymaps
nnoremap("gD", vim.lsp.buf.declaration)
nnoremap("gd", vim.lsp.buf.definition)
nnoremap("K", vim.lsp.buf.hover)
nnoremap("gi", vim.lsp.buf.implementation)
nnoremap("KK", vim.lsp.buf.signature_help)
nnoremap("[d", vim.diagnostic.goto_prev)
nnoremap("]d", vim.diagnostic.goto_next)
nnoremap("gl", vim.diagnostic.open_float)
leadernnoremap("x", vim.diagnostic.setloclist)

-- shut up
for _, key_fn in pairs({ nnoremap, inoremap, vnoremap, xnoremap }) do
	key_fn("<Up>", nil)
	key_fn("<Down>", nil)
	key_fn("<Right>", nil)
	key_fn("<Left>", nil)
end

vnoremap("<leader>gu", "<CMD>GitUrl<CR>")
