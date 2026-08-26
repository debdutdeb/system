local exists , blinkcmp = pcall(require, "blink.cmp")
if not exists then return end

blinkcmp.setup {
	keymap = { preset = "default" },
	signature = { enabled = true },
	completion = {
		documentation = { auto_show = false },
		menu = { auto_show = true },
		ghost_text = {
			enabled = true,
			show_with_menu = true,
		},

	},
	sources = {
      default = vim.tbl_extend("force", require("blink.cmp.config.sources"), { "lsp", "path", "snippets", "buffer" }),
      providers = {
        minuet = {
          name = "minuet",
          module = "minuet.blink",
          async = true,
          -- Should match minuet.config.request_timeout * 1000,
          -- since minuet.config.request_timeout is in seconds
          timeout_ms = 9000,
          score_offset = 50, -- Gives minuet higher priority among suggestions
        },
      },
    },
    fuzzy = { implementation = "lua" }
}
