if not vim then
	vim = {} -- this is just to shut up the lsp warnings
end

vim.opt.backup = false                          -- creates a backup file
vim.opt.clipboard = "unnamedplus"               -- allows neovim to access the system clipboard
vim.opt.cmdheight = 2                           -- more space in the neovim command line for displaying messages
vim.opt.completeopt = { "menuone", "noselect" } -- mostly just for cmp
vim.opt.conceallevel = 0                        -- so that `` is visible in markdown files
vim.opt.fileencoding = "utf-8"                  -- the encoding written to a file
vim.opt.hlsearch = true                         -- highlight all matches on previous search pattern
vim.opt.ignorecase = true                       -- ignore case in search patterns
vim.opt.mouse =
"a"                                             --  "disable the mouse (I hate it)" was my old opinion, now i don't care, rather it's proven helpful
vim.opt.pumheight = 10                          -- pop up menu height
vim.opt.showmode = false                        -- we don't need to see things like -- INSERT -- anymore (the font and colors are too much on my face)
vim.opt.showtabline = 1                         -- always show tabs
vim.opt.smartcase = true                        -- smart case
vim.opt.smartindent = true                      -- make indenting smarter again
vim.opt.splitbelow = true                       -- force all horizontal splits to go below current window
vim.opt.splitright = true                       -- force all vertical splits to go to the right of current window
vim.opt.swapfile = true                         -- creates a swapfile
vim.opt.termguicolors = true                    -- set term gui colors (most terminals support this)
vim.opt.timeoutlen = 500                        -- time to wait for a mapped sequence to complete (in milliseconds)
vim.opt.undofile = true                         -- enable persistent undo
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.updatetime = 50                         -- faster completion (4000ms default)
vim.opt.writebackup = false                     -- if a file is being edited by another program (or was written to file while editing with another program) it is not allowed to be edited
vim.opt.expandtab = false                       -- don't convert tabs to spaces
vim.opt.shiftwidth = 4                          -- the number of spaces inserted for each indentation
vim.opt.tabstop = 4                             -- insert 4 spaces for a tab
vim.opt.cursorline = true                       -- highlight the current line
vim.opt.number = true                           -- set numbered lines
vim.opt.relativenumber = true                   -- set relative numbered lines
vim.opt.numberwidth = 2                         -- set number column width to 2 {default 4}
vim.opt.signcolumn =
"yes"                                           -- always show the sign column, otherwise it would shift the text each time
vim.opt.wrap = true                             -- display lines as one long line
vim.opt.scrolloff = 8                           -- is one of my fav
vim.opt.sidescrolloff = 8
vim.opt.guifont = "UbuntuMono Nerd Font:h20"    -- the font used in graphical neovim applications -- namely neovide
vim.opt.colorcolumn = "100"
vim.opt.guicursor = ""

vim.opt.shortmess:append("c")

vim.opt.whichwrap:append("h,l")
vim.opt.iskeyword:append("-")

vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.g.netrw_bufsettings = "noma nomod nu nobl nowrap ro"
-- vim.g.netrw_liststyle = 3
vim.g.netrw_localcopydircmd = "cp -r"


vim.opt.grepprg = "rg --no-heading --column \"$*\""


vim.opt.winbar = "%{luaeval('vim.api.nvim_get_mode().mode')}%=r/o=%R,l=%L,c=%c,%%=%p,help=%H,preview=%W,ft=%Y%M"

vim.opt.statusline = "fname=%t,%<lines=%L,bufnr=%n,args=%a%=%{luaeval('vim.lsp.status()')}"

vim.cmd "colorscheme habamax"
