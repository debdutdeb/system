local Remap = require("chaos.keymaps")

local telescope_builtin = require("telescope.builtin")
local telescope_themes = require("telescope.themes")

local nnoremap = Remap.nnoremap
local inoremap = Remap.inoremap
local vnoremap = Remap.vnoremap
local xnoremap = Remap.xnoremap

local tsend_keys = Remap.tsend_keys

-- For wraps
nnoremap("j", 'v:count == 0 ? "gj" : "j"', { expr = true, silent = true })
nnoremap("k", 'v:count == 0 ? "gk" : "k"', { expr = true, silent = true })

-- Navigate buffers
nnoremap("<S-l>", "<cmd>bnext<CR>")
nnoremap("<S-h>", "<cmd>bprevious<CR>")

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

local function leadernnoremap(key, cmd, opts)
	nnoremap("<leader>" .. key, cmd, opts)
end

-- LSP
leadernnoremap("ll", "<cmd>LspStart<cr>")
leadernnoremap("lL", "<cmd>LspStartWithCmp<cr>")

leadernnoremap("o", "o<Esc>")
leadernnoremap("O", "O<Esc>")

local start_dir = vim.uv.cwd() -- load once

leadernnoremap("w", function() vim.opt_local.wrap = not vim.opt_local.wrap:get() end)
leadernnoremap("W", function() vim.o.wrap = not vim.opt.wrap:get() end)
leadernnoremap("q", "<cmd>q!<cr>")
leadernnoremap("c", "<cmd>bd!<cr>")

leadernnoremap("F", function()
end, { silent = false })
--leadernnoremap("e", "<cmd>Explore<cr>") -- open netrw


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

vnoremap("<leader>gu", "<CMD>'<,'>GitUrl!<CR><Esc>")
nnoremap("<leader>gu", "<CMD>GitUrl!<CR>")
require("chaos.fun")
vnoremap("<leader>cf", "<CMD>Freeze /tmp/freeze.png<CR>")

-- <cmd> is snappier than using ':', whY?; day 2: <cmd> doesn't wait for another key?
nnoremap("<Bs>", "<cmd>norm! $<cr>")
nnoremap("<Del>", "<cmd>norm! $<cr>")

leadernnoremap("f", {
	callback = function()
		-- ivy because it helps to actually read the contents better before moving into a file
		return telescope_builtin.find_files(
			telescope_themes.get_ivy({ border = false, layout_config = { prompt_position = "bottom" } })
		)
	end,
	fallback = function()
		local co = coroutine.create(function()
			local choices = require("fzf").fzf("fd -t f", "--multi", {
				relative = "cursor",
				border   = false,
				fzf_cwd  = start_dir,
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
	end,
})

leadernnoremap("b", {
	callback = function()
		return telescope_builtin.buffers(telescope_themes.get_cursor({ previewer = false, border = false }))
	end,
	fallback = function()
		tsend_keys(":ls<cr>:b ")
	end,
})

leadernnoremap("m", {
	callback = function()
		return telescope_builtin.marks(telescope_themes.get_cursor({ previewer = false, border = false }))
	end,
	fallback = function()
		vim.cmd(":marks<cr>")
	end,
})
leadernnoremap("sl", function()
	-- I didn't want a previewer for this, just the preview in the background text
	telescope_builtin.colorscheme(
		vim.tbl_deep_extend("force", { enable_preview = true }, telescope_themes.get_dropdown({ border = false }))
	)
end)
leadernnoremap("?", function()
	telescope_builtin.oldfiles({ border = false })
end)
leadernnoremap("F", {
	callback = function()
		return telescope_builtin.live_grep(
			telescope_themes.get_ivy({ border = false, layout_config = { prompt_position = "bottom" } })
		)
	end,
	fallback = function()
		tsend_keys(":silent lgrep! ")
	end,
})

leadernnoremap("la", vim.lsp.buf.code_action)
leadernnoremap("ld", {
	fallback = vim.diagnostic.setqflist,
	--callback = telescope_builtin.diagnostics,
	callback = "Telescope diagnostics",
})
leadernnoremap("lf", function()
	vim.lsp.buf.format({ async = true })
end)
leadernnoremap("lj", vim.lsp.diagnostic.goto_prev)
leadernnoremap("lk", vim.lsp.diagnostic.goto_next)
leadernnoremap("lc", vim.lsp.codelens.run)
leadernnoremap("lr", vim.lsp.buf.rename)
leadernnoremap("ls", {
	callback = function()
		return telescope_builtin.lsp_document_symbols(telescope_themes.get_dropdown({ border = false }))
	end,
	fallback = vim.lsp.buf.document_symbol
})
leadernnoremap("lS", {
	fallback = vim.lsp.buf.workspace_symbol,
	callback = function()
		return telescope_builtin.lsp_dynamic_workspace_symbols({ border = false })
	end,
})
leadernnoremap("lR", {
	fallack = vim.lsp.buf.references,
	callback = function()
		return telescope_builtin.lsp_references({ border = false })
	end,
})
leadernnoremap("lI", {
	fallack = vim.lsp.buf.implementation,
	callback = function()
		return telescope_builtin.lsp_implementations({ border = false })
	end,
})

leadernnoremap("gf", {
	callback = function() return telescope_builtin.git_files(telescope_themes.get_ivy({ border = false })) end,
	fallback = function()
		local files = {}
		for filename in string.gmatch(vim.system({ "git", "ls-files" }):wait().stdout, "([^\n]+)") do
			table.insert(files, { filename = filename, lnum = 1 })
		end
		vim.fn.setloclist(0, files)
	end,
})
leadernnoremap("gb", function()
	telescope_builtin.git_branches(telescope_themes.get_dropdown({ border = false }))
end)

-- Search namespace
leadernnoremap("sh", function()
	telescope_builtin.help_tags(telescope_themes.get_cursor({ previewer = false, border = false }))
end)
leadernnoremap("sm", function()
	telescope_builtin.man_pages(telescope_themes.get_cursor({ previewer = false, border = false }))
end)
leadernnoremap("sr", function()
	telescope_builtin.registers({ border = false })
end)
leadernnoremap("sk", function()
	telescope_builtin.keymaps(telescope_themes.get_cursor({ border = false }))
end)
leadernnoremap("sc", function()
	telescope_builtin.commands(telescope_themes.get_cursor({ border = false, previewer = false }))
end)

leadernnoremap("Sd", ":SessionsDeleteCurrent<cr>")

nnoremap("<A-g>.", ":G<cr>")
nnoremap("<A-g><space>", ":G! ", { silent = false })
-- git push
nnoremap("<A-g>pp", ":G! push origin HEAD<CR>")
nnoremap("<A-g>pf", ":G! push origin HEAD -f<CR>")
-- git status
nnoremap("<A-g>ss", ":G status --short %<cr>")
nnoremap("<A-g>sl", ":G status --long %<cr>")
-- git add
nnoremap("<A-g>ap", ":G add --patch %<cr>")
nnoremap("<A-g>af", ":G add %<cr>")
-- git diff
nnoremap("<A-g>ds", ":Gdiffsplit<cr>")
-- git commit
nnoremap("<A-g>cc", ":G commit<cr>")
nnoremap("<A-g>c<space>", ":G commit ", { silent = false })
nnoremap("<A-g>cm", ":G commit -m ", { silent = false })
