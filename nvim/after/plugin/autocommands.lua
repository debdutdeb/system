if vim == nil then vim = {} end

vim.cmd([[
augroup _auto_resize
autocmd!
autocmd VimResized * tabdo wincmd =
augroup end

autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

au BufRead,BufNewFile *.bash setfiletype bash
]])

local create_augroup = vim.api.nvim_create_augroup
local create_autocommand = vim.api.nvim_create_autocmd

local sessions = create_augroup("my/sessions-management", { clear = true })

--ac("VimLeave", {
--	callback = function(_)
--		if not vim.uv.fs_stat(".vim") then
--			vim.fn.mkdir(".vim")
--		end
--		vim.cmd(":mksession! " .. vim.fn.getcwd() .. "/.vim/session")
--	end,
--	group = sessions,
--})

create_autocommand("VimEnter", {
	callback = function(_)
		if vim.g.vscode then return end
		-- create a scratch buffer
		if vim.bo.filetype == "lazy" then return end

		local scratch_bufnr = vim.api.nvim_create_buf( --[[list this in bufferlist?]] true, --[[is this a scratch buffer?]]
			true)
		vim.api.nvim_buf_set_name(scratch_bufnr, "Scratch buffer")
		vim.bo[scratch_bufnr].filetype = "lua"
		--[[ local args = vim.api.nvim_command_output ":args"
		if args and args:match("%[.+%]") then
			return
		end ]]

		if vim.fn.argc(-1) > 0 then
			return
		end

		local schedule_load_or_default = setmetatable({
			fn = function() end,
			count = 0,
		}, {
			__call = function(self)
				self.fn()
			end
		})

		schedule_load_or_default.fn = function()
			vim.schedule(function()
				local ok, p = pcall(require, "persistence")
				if not ok then
					if schedule_load_or_default.count == 5 then
						vim.notify("persistence loading reached retry limit, not trying again")
						return
					end
					schedule_load_or_default.count = schedule_load_or_default.count + 1
					return schedule_load_or_default.fn()
				end

				local _, stat = pcall(vim.uv.fs_stat, p.current())
				if stat == nil then
					return require("telescope.builtin").find_files(require("telescope.themes").get_cursor({ border = true, layout_config = { width = { padding = 0 } } }))
				end

				p.load()
			end)
		end

		schedule_load_or_default()
		--if vim.uv.fs_stat(".vim/session") then
		--	vim.cmd ":source .vim/session"
		--else
		-- open a scratch buffer
		-- local no_buf = vim.api.nvim_get_current_buf()
		vim.cmd([[:buffer ]] .. scratch_bufnr)
		-- if vim.api.nvim_buf_get_name(no_buf) == "" then
		-- 	-- vim.cmd([[:bdelete ]] .. no_buf)
		-- end
		--end
	end,
	nested = true,
	group = sessions,
})

local folds = create_augroup("folds", { clear = true })

--vim.api.nvim_create_autocmd({ "BufWinLeave" }, {
--	pattern = { "*.*" },
--	desc = "save view (folds), when closing file",
--	command = "mkview",
--	group = folds,
--})
--
--vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
--	pattern = { "*.*" },
--	desc = "load view (folds), when opening file",
--	command = "silent! loadview",
--	group = folds,
--})

--[[ vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
	pattern = "*.tf",
	callback = function()
		vim.cmd "set ft=hcl"
	end
}) ]]

local qflist_group = create_augroup("my/qflist", { clear = true })

create_autocommand("QuickFixCmdPost", {
	pattern = "l*", -- locationlist
	command = "lopen",
	group = qflist_group,
})

create_autocommand("QuickFixCmdPost", {
	pattern = "[^l]*", -- quickfixlist
	command = "copen",
	group = qflist_group,
})


create_autocommand({ "TextYankPost" }, {
	callback = function()
		require('vim.highlight').on_yank({ higroup = 'Visual', timeout = 200 })
	end,
	group = create_augroup("my/yank-post-highlight", { clear = true }),
})

create_autocommand("FileType", {
	group = qflist_group,
	callback = function(_)
		vim.api.nvim_buf_set_keymap(0, "n", "q", ":q<cr>", { silent = true })
	end,
	pattern = { "qf", "help", "fugitive", "git" },
})
