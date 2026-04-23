# How to use this Neovim configuration

Leader key is **`<Space>`**.  Local leader is also `<Space>`.

---

## Installation

### With Nix (recommended)

```sh
# Run directly without installing
nix run github:debdutdeb/system?dir=nvim

# Or clone and run from the repo
nix run .

# Build the derivation (result/ symlink points to the binary)
nix build .
./result/bin/nvim
```

The Nix build pins every plugin to the exact commit in `lazy-lock.json` and
makes `cmake`, `rg`, and `fd` available on nvim's PATH automatically.  No
Mason installs are needed for the editor tooling itself (LSP servers and
formatters are still installed via Mason on first launch).

### Without Nix

Clone the repo and point `NVIM_APPNAME` at it, or symlink it to
`~/.config/nvim`.  On first launch lazy.nvim will bootstrap itself and install
all plugins automatically.

```sh
git clone <repo> ~/.config/nvim
nvim  # lazy.nvim clones everything on first open
```

---

## Startup

When no file is given on the command line, the editor tries to **restore the
last session** for the current git branch (via `persistence.nvim`).  If no
session exists it opens **Telescope find_files** so you can jump straight into
a file.  A scratch Lua buffer is always created in the background.

---

## Navigation

### Telescope  (`<leader>f`, `<leader>F`, …)

| Key | Action |
|-----|--------|
| `<leader>f` | Find files (fd-powered, ivy layout) |
| `<leader>F` | Live grep (rg-powered, ivy layout) |
| `<leader>b` | Open buffers |
| `<leader>m` | Marks |
| `<leader>?` | Recently opened files |
| `<leader>gf` | Git-tracked files |
| `<leader>gb` | Git branches |
| `<leader>sh` | Help tags |
| `<leader>sm` | Man pages |
| `<leader>sr` | Registers |
| `<leader>sk` | Keymaps |
| `<leader>sc` | Commands |
| `<leader>sl` | Preview and switch colorscheme |
| `<C-f>` | Copy relative path of any file (normal & insert) |

### Harpoon  (`<leader>a`, `<leader>h`, `<A-1..4>`)

Harpoon keeps a per-project shortlist of files you jump between constantly.

| Key | Action |
|-----|--------|
| `<leader>a` | Add current file to Harpoon list |
| `<leader>h` | Open the Harpoon quick-menu |
| `<A-1>` … `<A-4>` | Jump directly to slot 1–4 |
| `<C-n>` / `<C-p>` | Cycle to next / previous Harpoon entry |

### Buffers

| Key | Action |
|-----|--------|
| `<S-l>` | Next buffer |
| `<S-h>` | Previous buffer |
| `<leader>c` | Close (delete) current buffer |

---

## File exploration

| Key | Action |
|-----|--------|
| `<C-b>` | Toggle **Neo-tree** sidebar (right side) |
| `<leader>e` | Open **oil.nvim** for the current directory |

Neo-tree hijacks netrw for directory buffers.  oil.nvim is great for SSH
editing (`oil-ssh://`) and bulk renaming — edit the buffer like a text file and
`:w` to apply changes.

---

## LSP

Language servers are configured in `lua/debdut/lsp/init.lua`.  Most servers
have `autostart = false` so they do not start unless explicitly requested.

```vim
:LspStart          " start the server for the current filetype
```

`<leader>ll` is mapped to `:LspStart` and `<leader>lL` to `:LspStartWithCmp`.

### LSP keymaps

| Key | Action |
|-----|--------|
| `gd` | Go to definition |
| `gD` | Go to declaration |
| `gi` | Go to implementation |
| `K` | Hover documentation |
| `KK` | Signature help |
| `[d` / `]d` | Previous / next diagnostic |
| `gl` | Diagnostic float |
| `<leader>la` | Code actions |
| `<leader>ld` | Diagnostics (Telescope) |
| `<leader>lf` | Format buffer |
| `<leader>lr` | Rename symbol |
| `<leader>ls` | Document symbols |
| `<leader>lS` | Workspace symbols |
| `<leader>lR` | References |
| `<leader>lI` | Implementations |
| `<leader>lc` | Run code lens |
| `<leader>x` | Push diagnostics to location list |

Inlay hints are enabled automatically when the server supports them.
`lsp_signature.nvim` shows function signatures inline as you type.

### Installed servers (via Mason)

`yamlls`, `lua_ls`, and `ansiblels` start automatically.  All others
(`gopls`, `ts_ls`, `pyright`, `bashls`, `clangd`, `dockerls`, `html`,
`jsonls`, `perlnavigator`) start on demand with `:LspStart`.

Formatters installed: `prettier`, `black`, `shfmt`, `clang-format`, `biome`.  
Linters installed: `shellcheck`, `biome`, `ansible-lint`.

---

## Completion  (`nvim-cmp`)

| Key | Action |
|-----|--------|
| `<C-n>` | Select next item |
| `<C-p>` | Select previous item |
| `<C-y>` | Confirm selection |

Sources: LSP → path → buffer.  Snippets are provided by LuaSnip.

---

## Debugging  (`nvim-dap`, `nvim-dap-ui`)

The debug UI opens automatically when a session starts and closes when it ends.

| Key | Action |
|-----|--------|
| `<leader>dc` | Continue (also loads `.vscode/launch.json` if present) |
| `<leader>db` | Toggle breakpoint |
| `<leader>dj` | Step over |
| `<leader>di` | Step into |
| `<leader>do` | Step out |
| `<leader>d?` | Evaluate expression under cursor |

**Go** — uses `delve`.  Configurations: Debug file, Debug test, Debug module,
Attach to process.

**JavaScript / TypeScript** — uses `vscode-js-debug`.  Attach to a Node
process already running with `--inspect` on port 9229.

DAP reads `.vscode/launch.json` from the workspace root on each `:continue`
call, so per-project launch configs work out of the box.

---

## Git  (`vim-fugitive`, `gitsigns.nvim`)

Fugitive keymaps use `<A-g>` as a prefix:

| Key | Action |
|-----|--------|
| `<A-g>.` | `:G` — git status (fugitive buffer) |
| `<A-g><space>` | Run any git command interactively |
| `<A-g>pp` | Push current branch to origin |
| `<A-g>pf` | Force-push current branch |
| `<A-g>ap` | Stage hunks interactively (`git add --patch`) |
| `<A-g>af` | Stage whole file |
| `<A-g>cc` | Commit |
| `<A-g>cm` | Commit with inline message |
| `<A-g>ds` | Diff split |
| `<A-g>ss` | Git status short for current file |
| `<A-g>sl` | Git status long for current file |
| `<leader>gu` | Copy GitHub URL for current line / selection |

Fugitive and gitsigns only activate inside a git worktree (detected via
`chaos.nvim`).

---

## Editing helpers

### Surround  (`nvim-surround`)

| Key | Action |
|-----|--------|
| `ys<motion><char>` | Add surrounding |
| `ds<char>` | Delete surrounding |
| `cs<old><new>` | Change surrounding |
| `S<char>` (visual) | Surround selection |

### Comments  (`Comment.nvim`)

| Key | Mode | Action |
|-----|------|--------|
| `gcc` | Normal | Toggle line comment |
| `gc` | Visual | Toggle comment |
| `gb` | Visual | Toggle block comment |

Context-aware via `nvim-ts-context-commentstring` (JSX, Vue, etc.).

### Text movement

| Key | Mode | Action |
|-----|------|--------|
| `<A-J>` / `<A-K>` | Normal / Insert / Visual | Move line(s) down / up |
| `<` / `>` | Visual | Indent and stay in visual mode |
| `p` | Visual | Paste without clobbering the yank register |
| `<A-p>` / `<A-P>` | Normal | Paste on new line below / above |
| `jk` / `kj` | Insert | Exit to normal mode |
| `j` / `k` | Normal | `gj` / `gk` (respects visual wrapping) |
| `<Bs>` / `<Del>` | Normal | Jump to end of line |

### Find and replace

`<leader>r` (visual / visual-block) — yanks the selection into a register and
pre-fills `:%s/<selection>//g` so you only need to type the replacement.

---

## Project & session management

| Key | Action |
|-----|--------|
| `<leader>pd` | Discover projects under `~/git/` |
| `<leader>ph` | Browse project history |
| `<leader>pll` | Load a project |
| `<leader>plr` | Load most recent project |
| `<leader>plh` | Load project from history |
| `<leader>Sd` | Delete the current session |

Sessions are stored per git-branch (combining remote origin URL and branch
name), so switching branches restores the correct window layout.

---

## Treesitter

Parsers are compiled at runtime via `:TSInstall` (or automatically on first
open if `ensure_installed` includes the filetype).  The parser cache lives in
`~/.local/share/nvim/treesitter_parsers/`.

| Key | Action |
|-----|--------|
| `[c` | Jump up to the enclosing context |
| `gnn` | Begin incremental selection |
| `grn` | Expand node selection |
| `grc` | Expand to scope |
| `grm` | Shrink selection |
| `<C-n>` / `<C-p>` | Navigate to next / previous usage of symbol (refactor) |

The treesitter playground (`:TSPlaygroundToggle`) is available for inspecting
the parse tree.

---

## tmux integration  (`tmux.nvim`)

Clipboard is synced between tmux buffers and Neovim registers.  Pane navigation
uses the standard `<C-h/j/k/l>` bindings — the same keys work in both tmux
and Neovim without distinguishing which is focused.  Pane resizing uses
`<A-h/j/k/l>`.

---

## Misc

| Key | Action |
|-----|--------|
| `<leader>/` | Clear search highlight |
| `<leader>w` | Toggle line wrap (buffer-local) |
| `<leader>W` | Toggle line wrap (global) |
| `<C-b>` | Toggle Neo-tree |
| `<M-o>` / `<M-O>` | Insert blank line below / above without entering insert mode |
| Arrow keys | **Disabled** — use `hjkl` |

### Pomodoro

```vim
:PomodoroStart   " 30-min work timer
:PomodoroStatus  " check remaining time
```

### Presentations

```vim
:Presenting      " enter slide show mode (markdown / org headings as slides)
```

Inside a presentation: `n` next, `p` previous, `q` quit, `<CR>` next,
`<BS>` previous.

### MongoDB

```vim
:Mongo           " open the MongoDB browser (default: mongodb://localhost:27017)
```

---

## Colorscheme

Default: **hybrid** (dark, medium-contrast).  Switch interactively with
`<leader>sl`.  The active scheme is saved to `after/plugin/.colorscheme`.

---

## Configuration layout

```
nvim/
├── flake.nix                    # Nix package: pins all plugins + tools
├── lazy-lock.json               # Plugin version lock (used by flake.nix)
├── init.lua                     # Entry point
├── lua/
│   ├── debdut/
│   │   ├── options.lua          # Vim options, leader key
│   │   ├── plugins.lua          # lazy.nvim bootstrap / Nix-mode loader
│   │   ├── lsp/init.lua         # LSP server config + Mason setup
│   │   ├── dap/init.lua         # DAP adapters and keymaps
│   │   ├── treesitter.lua       # Treesitter options
│   │   ├── telescope.lua        # Telescope options
│   │   ├── harpoon.lua          # Harpoon setup + keymaps
│   │   └── comments.lua         # Comment.nvim options
│   └── my/plugins/              # One file per plugin group (lazy.nvim specs)
│       ├── lsp.lua              # nvim-lspconfig + mason
│       ├── completion.lua       # nvim-cmp + sources + LuaSnip
│       ├── treesitter.lua       # nvim-treesitter + extensions
│       ├── telescope.lua        # telescope + fzf-native
│       ├── git.lua              # fugitive + gitsigns
│       ├── dap.lua              # nvim-dap + dap-ui
│       ├── dap-js.lua           # JS/TS DAP via vscode-js-debug
│       └── …
└── after/plugin/
    ├── keymaps.lua              # All keymaps (loaded after plugins)
    └── autocommands.lua         # Autocommands (sessions, yank hl, qflist…)
```
