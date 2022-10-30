local Remap = require("neoconfig.keymap")
-- don't fail at startup
-- but it's ok to fail verbosely if later any function call fails
local _, telescope = pcall(require, "telescope")
local _, telescope_builtin = pcall(require, "telescope.builtin")
local _, telescope_themes = pcall(require, "telescope.themes")

local nnoremap = Remap.nnoremap
local inoremap = Remap.inoremap
local vnoremap = Remap.vnoremap
local xnoremap = Remap.xnoremap

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- Better window navigation
nnoremap("<C-h>", "<C-w>h")
nnoremap("<C-j>", "<C-w>j")
nnoremap("<C-k>", "<C-w>k")
nnoremap("<C-l>", "<C-w>l")

-- For wraps
nnoremap("j", 'v:count == 0 ? "gj" : "j"', { expr = true, silent = true })
nnoremap("k", 'v:count == 0 ? "gk" : "k"', { expr = true, silent = true })

-- Resize with arrows
nnoremap("<C-S-Up>", ":resize -2<CR>")
nnoremap("<C-S-Down>", ":resize +2<CR>")
nnoremap("<C-S-Left>", ":vertical resize -2<CR>")
nnoremap("<C-S-Right>", ":vertical resize +2<CR>")

-- Navigate buffers
nnoremap("<S-l>", ":bnext<CR>")
nnoremap("<S-h>", ":bprevious<CR>")

-- Move text up and down
nnoremap("<A-j>", "<Esc>:m .+1<CR>==gi")
nnoremap("<A-k>", "<Esc>:m .-2<CR>==gi")

--
-- nnoremap(":", telescope_builtin.commands(telescope_themes.get_ivy({ border = false })))

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

-- Terminal --
-- Better terminal navigation
-- keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
-- keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
-- keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
-- keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

local function leadernnoremap(key, cmd)
	nnoremap("<leader>" .. key, cmd)
end

-- LSP
leadernnoremap("li", "<cmd>LspInfo<cr>")
leadernnoremap("la", vim.lsp.buf.code_action)
leadernnoremap("ld", "<cmd>Telescope lsp_document_diagnostics<cr>")
leadernnoremap("lw", "<cmd>Telescope lsp_workspace_diagnostics<cr>")
leadernnoremap("lf", function()
	vim.lsp.buf.format({ async = true })
end)
leadernnoremap("lI", "<cmd>LspInstallInfo<cr>")
leadernnoremap("lR", "<cmd>LspRestart<cr>")
leadernnoremap("lj", vim.lsp.diagnostic.goto_prev)
leadernnoremap("lk", vim.lsp.diagnostic.goto_next)
leadernnoremap("lq", vim.lsp.diagnostic.set_loclist)
leadernnoremap("ll", vim.lsp.codelens.run)
leadernnoremap("lr", vim.lsp.buf.rename)
leadernnoremap("ls", "<cmd>Telescope lsp_document_symbols<cr>")
leadernnoremap("lS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>")

leadernnoremap("f", function()
	-- ivy because it helps to actually read the contents better before moving into a file
	telescope_builtin.find_files(telescope_themes.get_ivy({ border = false }))
end)
leadernnoremap("g", function()
	telescope_builtin.git_files(telescope_themes.get_ivy({ border = false }))
end)
leadernnoremap("b", function()
	-- I mean I'm not viewing a preview, why waste so much space?
	telescope_builtin.buffers(telescope_themes.get_cursor({ previewer = false, border = false }))
end)
leadernnoremap("cc", function()
	-- I didn't want a previewer for this, just the preview in the background text
	telescope_builtin.colorscheme(
		vim.tbl_deep_extend("force", { enable_preview = true }, telescope_themes.get_dropdown({ border = false }))
	)
end)
leadernnoremap("w", "<cmd>w!<cr>")
leadernnoremap("q", "<cmd>q!<cr>")
leadernnoremap("c", "<cmd>bd!<cr>")
leadernnoremap("F", function()
	-- definitely want to see as much as possible
	telescope_builtin.live_grep(telescope_themes.get_ivy({ border = false }))
end)
leadernnoremap("e", "<cmd>Ex<cr>") -- open netrw

-- Packer
leadernnoremap("pc", "<cmd>PackerCompile<cr>")
leadernnoremap("pi", "<cmd>PackerInstall<cr>")
leadernnoremap("ps", "<cmd>PackerSync<cr>")
leadernnoremap("pS", "<cmd>PackerStatus<cr>")
leadernnoremap("pu", "<cmd>PackerUpdate<cr>")

-- Telescope
leadernnoremap("sb", function()
	telescope_builtin.git_branches(telescope_themes.get_dropdown({ border = false }))
end)
-- leadernnoremap("sc", "<cmd>Telescope colorscheme<cr>")

-- following two, rather open a buffer to read than in through the previewer
leadernnoremap("sh", function()
	telescope_builtin.help_tags(telescope_themes.get_cursor({ previewer = false, border = false }))
end)
leadernnoremap("sM", function()
	telescope_builtin.man_pages(telescope_themes.get_cursor({ previewer = false, border = false }))
end)
leadernnoremap("sr", function()
	telescope_builtin.oldfiles(telescope_themes.get_ivy({ border = false }))
end)
leadernnoremap("sR", function()
	telescope_builtin.registers({ border = false })
end)
leadernnoremap("sk", function()
	telescope_builtin.keymaps(telescope_themes.get_cursor({ border = false }))
end)
leadernnoremap("sc", function()
	telescope_builtin.commands(telescope_themes.get_cursor({ border = false, previewer = false }))
end)

leadernnoremap("/", "<cmd>nohl<cr>")
