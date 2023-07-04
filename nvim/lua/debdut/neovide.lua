if not vim.g.neovide then
	return
end

vim.g.neovide_scale_factor = 0
vim.g.neovide_transparency = 0
vim.g.transparency = 0.9
vim.g.neovide_floating_blur_amount_x = 0.4
vim.g.neovide_floating_blur_amount_y = 0.4

vim.cmd([[
  let g:neovide_background_color = '#0f1117'.printf('%x', float2nr(255 * g:transparency))
  let g:neovide_underline_automatic_scaling = v:true
  let g:neovide_hide_mouse_when_typing = v:true
  let g:neovide_remember_window_size = v:true
  let g:neovide_cursor_vfx_mode = ""
  let g:neovide_input_macos_alt_is_meta = v:false
]])
-- vim.g.neovide_underline_automatic_scaling = "v:true"
-- vim.g.neovide_hide_mouse_when_typing = "v:true"
-- vim.cmd("let g:neovide_fullscreen = v:true")
