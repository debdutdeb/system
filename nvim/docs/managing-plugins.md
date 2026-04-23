# Managing plugins

Every plugin lives in three places:

| File | Purpose |
|------|---------|
| `flake.nix` | Source pin (GitHub URL + commit) and Nix derivation |
| `lua/my/plugins/<name>.lua` | lazy.nvim spec — config, keymaps, events, opts |

`flake.nix` has three sections that must all stay in sync for each plugin:

```
inputs.plugin-<slug>        ← GitHub URL pinned to a commit
p.<attr>  = mkPlugin ...    ← builds the vim plugin derivation
pluginDirsLua entry         ← tells lazy.nvim where the plugin lives on disk
```

---

## Adding a plugin

### 1. Get the commit you want to pin

```sh
# latest commit on the default branch
git ls-remote https://github.com/owner/repo HEAD
```

Or browse GitHub and copy the full SHA from the commit page.

### 2. Add the flake input

In `flake.nix`, inside the `inputs { }` block:

```nix
plugin-my-plugin = {
  url   = "github:owner/repo/FULL_COMMIT_SHA";
  flake = false;
};
```

Use the naming convention `plugin-<kebab-slug>`.  The slug just needs to be
unique — it does not have to match anything in the Lua config.

### 3. Add the derivation

In `flake.nix`, inside the `p = { ... }` block:

```nix
myPlugin = mkPlugin "my-plugin.nvim" inputs.plugin-my-plugin;
```

The string passed to `mkPlugin` (`"my-plugin.nvim"`) becomes the name of the
Nix derivation.  It does **not** need to match the repo name, but matching it
makes things readable.

If the plugin requires a build step (e.g. cmake, make), override the
derivation instead:

```nix
myPlugin = pkgs.vimUtils.buildVimPlugin {
  name             = "my-plugin.nvim";
  src              = inputs.plugin-my-plugin;
  nativeBuildInputs = [ pkgs.cmake ];
  buildPhase       = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build";
  doCheck          = false;
};
```

### 4. Add the `pluginDirsLua` entry

In `flake.nix`, inside the `pluginDirsLua = pkgs.writeText ... ''` block,
add one line.  The first string must match what lazy.nvim uses to identify the
plugin — that is `"owner/repo"` exactly as written in the lazy spec.

```nix
{ "owner/repo",  dir = "${p.myPlugin}" },
```

If your lazy spec sets a custom `name = "something"`, include it here too so
lazy.nvim can merge the specs by the same identity:

```nix
{ "owner/repo", name = "something", dir = "${p.myPlugin}" },
```

### 5. Write the lazy spec

Create `lua/my/plugins/my-plugin.lua`:

```lua
return {
  "owner/repo",
  event = "VeryLazy",   -- or keys / ft / cmd / lazy = false
  opts  = {
    option_a = true,
  },
  -- or config = function() require("my-plugin").setup({ ... }) end
}
```

### 6. Rebuild

```sh
nix build .
./result/bin/nvim
```

`flake.lock` is updated automatically with the hash for the new input.

---

## Removing a plugin

### 1. Remove or empty the lazy spec

Delete `lua/my/plugins/<name>.lua`, or return an empty table if the file
is imported elsewhere:

```lua
return {}
```

### 2. Remove the three flake.nix entries

- The `inputs.plugin-<slug>` line
- The `p.<attr>` line in the derivation block
- The `{ "owner/repo", dir = ... }` line in `pluginDirsLua`

### 3. Remove any keymaps or config that referenced the plugin

Check `after/plugin/keymaps.lua` and any other files that `require` the plugin.

### 4. Rebuild

```sh
nix build .
```

---

## Updating a plugin to a newer commit

Change only the commit SHA in the flake input:

```nix
# before
plugin-my-plugin = { url = "github:owner/repo/abc123..."; flake = false; };

# after
plugin-my-plugin = { url = "github:owner/repo/def456..."; flake = false; };
```

Then rebuild.  `flake.lock` is recomputed automatically.  You do not need to
touch the lazy spec or `pluginDirsLua`.

---

## Plugins with a custom `name` in the lazy spec

Some specs use `name = "..."` to give lazy.nvim an internal alias.  The
`pluginDirsLua` entry must carry the same `name` field so the two specs merge
correctly (lazy identifies plugins by name, not by repo slug).

Examples already in this config:

| Lazy spec | `pluginDirsLua` entry |
|-----------|----------------------|
| `{ "nvim-treesitter/nvim-treesitter", name = "ts" }` | `{ "nvim-treesitter/nvim-treesitter", name = "ts", dir = "..." }` |
| `{ "nvim-treesitter/nvim-treesitter-context", name = "treesitter-context" }` | `{ ..., name = "treesitter-context", dir = "..." }` |
| `{ "mxsdev/nvim-dap-vscode-js", name = "dap-vscode-js" }` | `{ ..., name = "dap-vscode-js", dir = "..." }` |

---

## Non-Nix fallback

When `$NIX_NVIM_PLUGIN_DIRS` is not set (i.e. you launch the system `nvim`
instead of `./result/bin/nvim`), `plugins.lua` falls back to the original
lazy.nvim git-clone bootstrap.  In that mode lazy.nvim downloads plugins
normally using `lazy-lock.json` for version pinning — no Nix involvement.

This means you can still iterate on Lua config quickly without rebuilding:
edit a file, open the system nvim, see the result immediately.  Rebuild only
when you add, remove, or update a plugin.
