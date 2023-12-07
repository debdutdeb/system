local Remap = require("debdut.keymap")

local nnoremap = Remap.nnoremap
local inoremap = Remap.inoremap
local vnoremap = Remap.vnoremap
local xnoremap = Remap.xnoremap

local tsend_keys = Remap.tsend_keys
local nsend_keys = Remap.nsend_keys

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

-- Visual --
-- Stay in indent mode
vnoremap("<", "<gv")
vnoremap(">", ">gv")

-- Move text up and down
vnoremap("<A-j>", ":m .+1<CR>==")
vnoremap("<A-k>", ":m .-2<CR>==")
vnoremap("p", '"_dP')

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
leadernnoremap("la", vim.lsp.buf.code_action)
leadernnoremap("ld", vim.diagnostic.show)
leadernnoremap("lw", function() vim.cmd("echo noop") end)
leadernnoremap("lf", function()
	vim.lsp.buf.format({ async = true })
end)
leadernnoremap("lj", vim.lsp.diagnostic.goto_prev)
leadernnoremap("lk", vim.lsp.diagnostic.goto_next)
leadernnoremap("lq", vim.lsp.set_loclist)
leadernnoremap("ll", vim.lsp.codelens.run)
leadernnoremap("lr", vim.lsp.buf.rename)
leadernnoremap("lS", vim.lsp.buf.workspace_symbol)
nnoremap("lR", vim.lsp.buf.references)
nnoremap("lI", vim.lsp.buf.implementation)

leadernnoremap("g", function()
	local files = {}
	for filename in string.gmatch(vim.system({ "git", "ls-files" }):wait().stdout, "([^\n]+)") do
		table.insert(files, { filename = filename, lnum = 1 })
	end
	vim.fn.setqflist(files)
	vim.cmd ":copen"
end)

leadernnoremap("b", function()
	tsend_keys(":ls<cr>:b ")
end, { silent = false })

leadernnoremap("w", "<cmd>w!<cr>")
leadernnoremap("q", "<cmd>q!<cr>")
leadernnoremap("c", "<cmd>bd!<cr>")
leadernnoremap("f", "<cmd>FZF<cr>")
leadernnoremap("F", function()
	tsend_keys(":silent grep! ")
end, { silent = false })
leadernnoremap("e", "<cmd>Sex<cr>") -- open netrw


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
