--[[ https://github.com/neovim/nvim-lspconfig/tree/master/lua/lspconfig/server_configurations ]]

vim.diagnostic.config({
	virtual_text = true,
	update_in_insert = true,
	underline = true,
	severity_sort = true,
	float = {
		source = "if_many",
		header = "",
		prefix = "",
	},
})

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = nil,
	width = 60,
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = nil,
	width = 60,
})

local default_opts = {
	autostart = true,
}

function default_opts.on_attach(client, bufnr)
	if client.server_capabilities.inlayHintProvider ~= nil and client.server_capabilities.inlayHintProvider then
		vim.lsp.inlay_hint(bufnr)
	end
end

default_opts.capabilities = vim.lsp.protocol.make_client_capabilities()
--completion capabilities
local ok, coq = pcall(require, "coq")
if ok then default_opts.capabilities = coq.lsp_ensure_capabilities(default_opts.capabilities) end

local function root_directory_pattern(pattern)
	vim.validate({ _ = { pattern, "table" } })

	return vim.fs.dirname(vim.fs.find(pattern, { upward = true })[1]) or vim.fn.getcwd()
end

local function startlspfn(opts)
	return function() vim.lsp.start(vim.tbl_deep_extend("force", opts, default_opts)) end
end

local callbacks = {}

callbacks = setmetatable(callbacks, {
	__index = function(self, _ft) return function() end end,

	__newindex = function(self, ft, fn)
		vim.validate({
			ft = { ft, { "string", "table" } },
			fn = { fn, "function" },
		})

		if type(ft) ~= "table" then ft = { ft } end

		for _, _ft in ipairs(ft) do rawset(self, _ft, fn) end
	end,
})


callbacks[{ "c", "cpp" }] = startlspfn({
	cmd = { "clangd" },
	root_dir = vim.fn.getcwd(),
})

callbacks[{ "go", "gomod", "gotmpl" }] = startlspfn({
	cmd = { "gopls" },
	root_dir = root_directory_pattern({ "go.mod", ".git" }),
	settings = {
		gopls = {
			hints = {
				assignVariableTypes = true,
				compositeLiteralFields = true,
				constantValues = true,
				functionTypeParameters = true,
				parameterNames = true,
				rangeVariableTypes = true,
			},
		},
	},
})

callbacks["lua"] = startlspfn({
	cmd = { "lua-language-server" },
	root_dir = root_directory_pattern({ ".luarc.json", ".luarc.jsonc", ".stylua.toml", "stylua.toml" }),
	single_file_support = true,
	log_level = vim.lsp.protocol.MessageType.Warning,
})

callbacks[{ "javascript", "typescript", "javascriptreact", "typescriptreact", "javascript.jsx", "typescript.tsx" }] =
	startlspfn({
		cmd = { "typescript-language-server", "--stdio" },
		root_dir = root_directory_pattern({ "package.json", "node_modules", "yarn.lock", "package-lock.json",
			"tsconfig.json" }),
		single_file_support = true,
		settings = {
			typescript = {
				inlayHints = {
					includeInlayParameterNameHints = "all",
					includeInlayParameterNameHintsWhenArgumentMatchesName = false,
					includeInlayFunctionParameterTypeHints = true,
					includeInlayVariableTypeHints = true,
					includeInlayPropertyDeclarationTypeHints = true,
					includeInlayFunctionLikeReturnTypeHints = true,
					includeInlayEnumMemberValueHints = true,
				},
			},
			javascript = {
				inlayHints = {
					includeInlayParameterNameHints = "all",
					includeInlayParameterNameHintsWhenArgumentMatchesName = false,
					includeInlayFunctionParameterTypeHints = true,
					includeInlayVariableTypeHints = true,
					includeInlayPropertyDeclarationTypeHints = true,
					includeInlayFunctionLikeReturnTypeHints = true,
					includeInlayEnumMemberValueHints = true,
				},
			},
		},
		on_attach = function(client, bufnr)
			client.server_capabilities.document_formatting = false
			default_opts.on_attach(client, bufnr)
		end,
	})

callbacks[{ "terraform" }] = startlspfn({
	cmd = { "terraform-ls", "serve" },
	root_dir = vim.uv.cwd(),
	single_file_support = true,
})

return callbacks
