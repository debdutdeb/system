vim.cmd([[
augroup _general_settings
autocmd!
autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
autocmd TextYankPost * silent!lua require('vim.highlight').on_yank({higroup = 'Visual', timeout = 200})
autocmd FileType qf set nobuflisted
augroup end

augroup _git
autocmd!
autocmd FileType gitcommit setlocal wrap
autocmd FileType gitcommit setlocal spell
augroup end

augroup _markdown
autocmd!
autocmd FileType markdown setlocal wrap
autocmd FileType markdown setlocal spell
augroup end

augroup _auto_resize
autocmd!
autocmd VimResized * tabdo wincmd =
augroup end

autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

au BufRead,BufNewFile *.bash setfiletype bash

augroup remember_folds
autocmd!
au BufWinLeave ?* mkview 1
au BufWinEnter ?* silent! loadview 1
augroup END
]])

vim.api.nvim_create_autocmd("VimEnter", {
	callback = function()
		if vim.api.nvim_get_option_value("filetype", {}) == "netrw" then
			return
		end
		if vim.api.nvim_buf_get_name(0) == "" then
			local ok, telescope = pcall(require, "telescope.builtin")
			if not ok then
				return
			end
			local ok, themes = pcall(require, "telescope.themes")
			if not ok then
				telescope.find_files()
				return
			end
			telescope.find_files(themes.get_cursor({ border = false, layout_config = { height = 15 } }))
		end
	end,
})

--[[ local lspconfig_configs = require("lspconfig.configs")
local lspconfig_util = require("lspconfig.util")
local plenary_filetype = require("plenary.filetype")
local lsp_servers_configured = require("debdut.lsp.servers")

local function __maybe_start_lsp(args)
	-- TODO this probably can be simplified couple folds given lsp.start exists
	local autostart_patterns = {
		"neovim_autostart_lsp",
		"vim_autostart_lsp",
		"autostart_lsp",
		"lsp_autostart",
		"start_lsp",
		"lsp_start",
	}
	local ft = plenary_filetype.detect(args.file or args.match)
	for _, client in pairs(lspconfig_util.get_active_clients_list_by_ft(ft)) do
		if client ~= "null-ls" then
			return
		end
	end
	local get_configured_server = function()
		for _, client in pairs(lspconfig_util.get_other_matching_providers(ft)) do
			for _, server in pairs(lsp_servers_configured) do
				if server == client.name then
					return server
				end
			end
		end
	end
	local config = lspconfig_configs[get_configured_server()]
	if not config then
		return
	end
	-- try to find the trigger files in current and parents before lsp root
	if lspconfig_util.root_pattern(unpack(autostart_patterns))(args.match or args.file) ~= nil then
		return config.launch(args.buf)
	end
	coroutine.resume(coroutine.create(function()
		local root_dir
		local status, error = pcall(function()
			root_dir = config.get_root_dir(args.match)
		end)
		if vim.in_fast_event() then
			local routine = assert(coroutine.running())
			vim.schedule(function()
				coroutine.resume(routine)
			end)
			routine.yield()
		end
		if not status then
			return vim.notify_once(
				("[config] failed to start language server %s::error: %s"):format(language_server, error),
				vim.log.levels.WARN
			)
		end
		if not root_dir then
			return
		end

		if #vim.fs.find(autostart_patterns, { upward = false, limit = 1, type = "file", path = root_dir }) ~= 0 then
			-- config.launch(args.buf)
			vim.lsp.start(config)
		end
	end))
end

vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function(args)
		vim.defer_fn(function()
			__maybe_start_lsp(args)
		end, 2000)
	end,
	-- https://github.com/neovim/nvim-lspconfig/blob/0011c435282f043a018e23393cae06ed926c3f4a/lua/lspconfig/configs.lua#L64
	group = vim.api.nvim_create_augroup("lspconfig", { clear = false }),
}) ]]
