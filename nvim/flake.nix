{
  description = "Neovim configuration with Nix-managed plugins (pinned in flake inputs)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # ---------------------------------------------------------------------------
    # Plugin sources — each rev comes directly from lazy-lock.json
    # ---------------------------------------------------------------------------
    plugin-comment-nvim          = { url = "github:numToStr/Comment.nvim/e30b7f2008e52442154b66f7c519bfd2f1e32acb";                      flake = false; };
    plugin-luasnip               = { url = "github:L3MON4D3/LuaSnip/de10d8414235b0a8cabfeba60d07c24304e71f5c";                           flake = false; };
    plugin-schemastore            = { url = "github:b0o/SchemaStore.nvim/9ecd375f7c261f622bd811ecebcddd59bf5e9de2";                        flake = false; };
    plugin-chaos-nvim            = { url = "github:debdutdeb/chaos.nvim/7947051124f34d554f76a9878b9d408c7e94c676";                        flake = false; };
    plugin-cmp-buffer            = { url = "github:hrsh7th/cmp-buffer/b74fab3656eea9de20a9b8116afa3cfc4ec09657";                          flake = false; };
    plugin-cmp-nvim-lsp          = { url = "github:hrsh7th/cmp-nvim-lsp/a8912b88ce488f411177fc8aed358b04dc246d7b";                       flake = false; };
    plugin-cmp-path              = { url = "github:hrsh7th/cmp-path/c642487086dbd9a93160e1679a1327be111cbc25";                            flake = false; };
    plugin-cmp-luasnip           = { url = "github:saadparwaiz1/cmp_luasnip/98d9cb5c2c38532bd9bdb481067b20fea8f32e90";                   flake = false; };
    plugin-dap-vscode-js         = { url = "github:mxsdev/nvim-dap-vscode-js/03bd29672d7fab5e515fc8469b7d07cc5994bbf6";                  flake = false; };
    plugin-fzf-lua               = { url = "github:ibhagwan/fzf-lua/32678d1151843ea5ac4568419cd0c5ee2cdf61a9";                            flake = false; };
    plugin-gitsigns              = { url = "github:lewis6991/gitsigns.nvim/736f51d2bb684c06f39a2032f064d7244f549981";                     flake = false; };
    plugin-harpoon               = { url = "github:ThePrimeagen/harpoon/ed1f853847ffd04b2b61c314865665e1dadf22c7";                        flake = false; };
    plugin-hybrid-nvim           = { url = "github:HoNamDuong/hybrid.nvim/74dfee0d5084a3db5e2ad0a78a67ee45e93a64bf";                     flake = false; };
    plugin-lsp-signature         = { url = "github:ray-x/lsp_signature.nvim/fc38521ea4d9ec8dbd4c2819ba8126cea743943b";                   flake = false; };
    plugin-lua-events            = { url = "github:simenkid/lua-events/228a9f04dc3ef7bc525e723ceaf9e593c6598ef3";                         flake = false; };
    plugin-mason-lspconfig       = { url = "github:williamboman/mason-lspconfig.nvim/1a31f824b9cd5bc6f342fc29e9a53b60d74af245";           flake = false; };
    plugin-mason-null-ls         = { url = "github:jay-babu/mason-null-ls.nvim/a991e7697514f30126fac7c07a11c488c553e94f";                 flake = false; };
    plugin-mason                 = { url = "github:williamboman/mason.nvim/7dc4facca9702f95353d5a1f87daf23d78e31c2a";                     flake = false; };
    plugin-mongo-nvim            = { url = "github:donus3/mongo.nvim/8583f64636ddc84a12ef8c31a3b28536620ced9c";                           flake = false; };
    plugin-neo-tree              = { url = "github:nvim-neo-tree/neo-tree.nvim/cea666ef965884414b1b71f6b39a537f9238bdb2";                 flake = false; };
    plugin-neodev                = { url = "github:folke/neodev.nvim/46aa467dca16cf3dfe27098042402066d2ae242d";                           flake = false; };
    plugin-neovim-project        = { url = "github:coffebar/neovim-project/d694c9760dbc443ea1ee09b36e774d7a1085f557";                     flake = false; };
    plugin-neovim-session-manager = { url = "github:Shatur/neovim-session-manager/3409dc920d40bec4c901c0a122a80bee03d6d1e1";              flake = false; };
    plugin-none-ls-extras        = { url = "github:nvimtools/none-ls-extras.nvim/924fe88a9983c7d90dbb31fc4e3129a583ea0a90";               flake = false; };
    plugin-none-ls               = { url = "github:nvimtools/none-ls.nvim/5fcb73913a9290f78097e34420fe0e6130c5c33c";                      flake = false; };
    plugin-nui                   = { url = "github:MunifTanjim/nui.nvim/de740991c12411b663994b2860f1a4fd0937c130";                        flake = false; };
    plugin-nvim-cmp              = { url = "github:hrsh7th/nvim-cmp/8c82d0bd31299dbff7f8e780f5e06d2283de9678";                            flake = false; };
    plugin-nvim-dap              = { url = "github:mfussenegger/nvim-dap/a479e25ed5b5d331fb46ee4b9e160ff02ac64310";                       flake = false; };
    plugin-nvim-dap-ui           = { url = "github:rcarriga/nvim-dap-ui/cf91d5e2d07c72903d052f5207511bf7ecdb7122";                        flake = false; };
    plugin-nvim-dap-virtual-text = { url = "github:theHamsta/nvim-dap-virtual-text/fbdb48c2ed45f4a8293d0d483f7730d24467ccb6";             flake = false; };
    plugin-nvim-lspconfig        = { url = "github:neovim/nvim-lspconfig/9141be4c1332afc83bdf1b0278dbb030f75ff8e3";                       flake = false; };
    plugin-nvim-nio              = { url = "github:nvim-neotest/nvim-nio/21f5324bfac14e22ba26553caf69ec76ae8a7662";                       flake = false; };
    plugin-nvim-surround         = { url = "github:kylechui/nvim-surround/0d6882635817a2677749a330127d12ac30a4f3c8";                      flake = false; };
    plugin-nvim-treesitter-refactor = { url = "github:nvim-treesitter/nvim-treesitter-refactor/d8b74fa87afc6a1e97b18da23e762efb032dc270"; flake = false; };
    plugin-nvim-ts-autotag       = { url = "github:windwp/nvim-ts-autotag/a1d526af391f6aebb25a8795cbc05351ed3620b5";                      flake = false; };
    plugin-nvim-ts-context-commentstring = { url = "github:JoosepAlviste/nvim-ts-context-commentstring/1b212c2eee76d787bbea6aa5e92a2b534e7b4f8f"; flake = false; };
    plugin-nvim-web-devicons     = { url = "github:nvim-tree/nvim-web-devicons/3362099de3368aa620a8105b19ed04c2053e38c0";                 flake = false; };
    plugin-oil                   = { url = "github:stevearc/oil.nvim/bbad9a76b2617ce1221d49619e4e4b659b3c61fc";                           flake = false; };
    plugin-org-bullets           = { url = "github:akinsho/org-bullets.nvim/21437cfa99c70f2c18977bffd423f912a7b832ea";                   flake = false; };
    plugin-orgmode               = { url = "github:nvim-orgmode/orgmode/2b91d9ab8688b5df9436aa7340b7a477c4b336d4";                        flake = false; };
    plugin-overseer              = { url = "github:stevearc/overseer.nvim/fe7b2f9ba263e150ab36474dfc810217b8cf7400";                      flake = false; };
    plugin-persistence           = { url = "github:folke/persistence.nvim/166a79a55bfa7a4db3e26fc031b4d92af71d0b51";                      flake = false; };
    plugin-playground            = { url = "github:nvim-treesitter/playground/ba48c6a62a280eefb7c85725b0915e021a1a0749";                  flake = false; };
    plugin-plenary               = { url = "github:nvim-lua/plenary.nvim/b9fd5226c2f76c951fc8ed5923d85e4de065e509";                      flake = false; };
    plugin-pomodoro              = { url = "github:wthollingsworth/pomodoro.nvim/04ea4152b1e1d0a42ac95f9f527a7cd9adec59f2";               flake = false; };
    plugin-presenting            = { url = "github:sotte/presenting.nvim/ab2fe3ee7edd990c3b3fa41b833b33a2f66fcf3a";                       flake = false; };
    plugin-telescope-fzf-native  = { url = "github:nvim-telescope/telescope-fzf-native.nvim/1f08ed60cafc8f6168b72b80be2b2ea149813e55";   flake = false; };
    plugin-telescope-orgmode     = { url = "github:nvim-orgmode/telescope-orgmode.nvim/b445c090c9b5fbdce390e015853364b0b9df5e5a";         flake = false; };
    plugin-telescope             = { url = "github:nvim-telescope/telescope.nvim/7011eaae0ac1afe036e30c95cf80200b8dc3f21a";               flake = false; };
    plugin-tmux-nvim             = { url = "github:aserowy/tmux.nvim/2c1c3be0ef287073cef963f2aefa31a15c8b9cd8";                          flake = false; };
    plugin-treesitter-context    = { url = "github:nvim-treesitter/nvim-treesitter-context/dca8726fea2c14e1ce6adbaa76a04816fbfaff61";     flake = false; };
    plugin-nvim-treesitter       = { url = "github:nvim-treesitter/nvim-treesitter/42fc28ba918343ebfd5565147a42a26580579482";             flake = false; };
    plugin-vim-abolish           = { url = "github:tpope/vim-abolish/dcbfe065297d31823561ba787f51056c147aa682";                           flake = false; };
    plugin-vim-fugitive          = { url = "github:tpope/vim-fugitive/61b51c09b7c9ce04e821f6cf76ea4f6f903e3cf4";                          flake = false; };
    plugin-vim-ghost             = { url = "github:raghur/vim-ghost/115e2600481c92c0bfb69d82ccbd8af7dc052a03";                            flake = false; };
    plugin-vim-table-mode        = { url = "github:dhruvasagar/vim-table-mode/e4365bde024f73e205eefa2fb78e3029ddb92ea9";                  flake = false; };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSystem = f: nixpkgs.lib.genAttrs systems f;
    in
    {
      packages = forEachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          mkPlugin = name: src:
            pkgs.vimUtils.buildVimPlugin { inherit name src; doCheck = false; };

          # ------------------------------------------------------------------
          # Plugin derivations
          # ------------------------------------------------------------------
          p = {
            commentNvim           = mkPlugin "Comment.nvim"                      inputs.plugin-comment-nvim;
            luasnip               = mkPlugin "LuaSnip"                           inputs.plugin-luasnip;
            schemastore           = mkPlugin "SchemaStore.nvim"                  inputs.plugin-schemastore;
            chaosNvim             = mkPlugin "chaos.nvim"                        inputs.plugin-chaos-nvim;
            cmpBuffer             = mkPlugin "cmp-buffer"                        inputs.plugin-cmp-buffer;
            cmpNvimLsp            = mkPlugin "cmp-nvim-lsp"                      inputs.plugin-cmp-nvim-lsp;
            cmpPath               = mkPlugin "cmp-path"                          inputs.plugin-cmp-path;
            cmpLuasnip            = mkPlugin "cmp_luasnip"                       inputs.plugin-cmp-luasnip;
            dapVscodeJs           = mkPlugin "dap-vscode-js"                     inputs.plugin-dap-vscode-js;
            fzfLua                = mkPlugin "fzf-lua"                           inputs.plugin-fzf-lua;
            gitsigns              = mkPlugin "gitsigns.nvim"                     inputs.plugin-gitsigns;
            harpoon               = mkPlugin "harpoon"                           inputs.plugin-harpoon;
            hybridNvim            = mkPlugin "hybrid.nvim"                       inputs.plugin-hybrid-nvim;
            lspSignature          = mkPlugin "lsp_signature.nvim"                inputs.plugin-lsp-signature;
            luaEvents             = mkPlugin "lua-events"                        inputs.plugin-lua-events;
            masonLspconfig        = mkPlugin "mason-lspconfig.nvim"              inputs.plugin-mason-lspconfig;
            masonNullLs           = mkPlugin "mason-null-ls.nvim"                inputs.plugin-mason-null-ls;
            mason                 = mkPlugin "mason.nvim"                        inputs.plugin-mason;
            mongoNvim             = mkPlugin "mongo.nvim"                        inputs.plugin-mongo-nvim;
            neoTree               = mkPlugin "neo-tree.nvim"                     inputs.plugin-neo-tree;
            neodev                = mkPlugin "neodev.nvim"                       inputs.plugin-neodev;
            neovimProject         = mkPlugin "neovim-project"                    inputs.plugin-neovim-project;
            neovimSessionManager  = mkPlugin "neovim-session-manager"            inputs.plugin-neovim-session-manager;
            noneLsExtras          = mkPlugin "none-ls-extras.nvim"               inputs.plugin-none-ls-extras;
            noneLs                = mkPlugin "none-ls.nvim"                      inputs.plugin-none-ls;
            nui                   = mkPlugin "nui.nvim"                          inputs.plugin-nui;
            nvimCmp               = mkPlugin "nvim-cmp"                          inputs.plugin-nvim-cmp;
            nvimDap               = mkPlugin "nvim-dap"                          inputs.plugin-nvim-dap;
            nvimDapUi             = mkPlugin "nvim-dap-ui"                       inputs.plugin-nvim-dap-ui;
            nvimDapVirtualText    = mkPlugin "nvim-dap-virtual-text"             inputs.plugin-nvim-dap-virtual-text;
            nvimLspconfig         = mkPlugin "nvim-lspconfig"                    inputs.plugin-nvim-lspconfig;
            nvimNio               = mkPlugin "nvim-nio"                          inputs.plugin-nvim-nio;
            nvimSurround          = mkPlugin "nvim-surround"                     inputs.plugin-nvim-surround;
            nvimTreesitterRefactor = mkPlugin "nvim-treesitter-refactor"         inputs.plugin-nvim-treesitter-refactor;
            nvimTsAutotag         = mkPlugin "nvim-ts-autotag"                   inputs.plugin-nvim-ts-autotag;
            nvimTsContextCommentstring = mkPlugin "nvim-ts-context-commentstring" inputs.plugin-nvim-ts-context-commentstring;
            nvimWebDevicons       = mkPlugin "nvim-web-devicons"                 inputs.plugin-nvim-web-devicons;
            oil                   = mkPlugin "oil.nvim"                          inputs.plugin-oil;
            orgBullets            = mkPlugin "org-bullets.nvim"                  inputs.plugin-org-bullets;
            orgmode               = mkPlugin "orgmode"                           inputs.plugin-orgmode;
            overseer              = mkPlugin "overseer.nvim"                     inputs.plugin-overseer;
            persistence           = mkPlugin "persistence.nvim"                  inputs.plugin-persistence;
            playground            = mkPlugin "playground"                        inputs.plugin-playground;
            plenary               = mkPlugin "plenary.nvim"                      inputs.plugin-plenary;
            pomodoro              = mkPlugin "pomodoro.nvim"                     inputs.plugin-pomodoro;
            presenting            = mkPlugin "presenting.nvim"                   inputs.plugin-presenting;
            telescopeOrgmode      = mkPlugin "telescope-orgmode.nvim"            inputs.plugin-telescope-orgmode;
            telescope             = mkPlugin "telescope.nvim"                    inputs.plugin-telescope;
            tmuxNvim              = mkPlugin "tmux.nvim"                         inputs.plugin-tmux-nvim;
            treesitterContext     = mkPlugin "treesitter-context"                inputs.plugin-treesitter-context;
            nvimTreesitter        = mkPlugin "ts"                                inputs.plugin-nvim-treesitter;
            vimAbolish            = mkPlugin "vim-abolish"                       inputs.plugin-vim-abolish;
            vimFugitive           = mkPlugin "vim-fugitive"                      inputs.plugin-vim-fugitive;
            vimGhost              = mkPlugin "vim-ghost"                         inputs.plugin-vim-ghost;
            vimTableMode          = mkPlugin "vim-table-mode"                    inputs.plugin-vim-table-mode;

            # telescope-fzf-native requires a cmake build step
            telescopeFzfNative = pkgs.vimUtils.buildVimPlugin {
              name = "telescope-fzf-native.nvim";
              src = inputs.plugin-telescope-fzf-native;
              nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config ];
              buildPhase = ''
                cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release -DCMAKE_POLICY_VERSION_MINIMUM=3.5
                cmake --build build --config Release
                cmake --install build --prefix build
              '';
              doCheck = false;
            };
          };

          # ------------------------------------------------------------------
          # Map of short keys -> Nix store paths, read by debdut/plugins.lua.
          # Keys and startup order are defined in lua/debdut/plugins.lua.
          # ------------------------------------------------------------------
          pluginDirsLua = pkgs.writeText "nix-plugin-dirs.lua" ''
            return {
              plenary = [[${p.plenary}]],
              nvimNio = [[${p.nvimNio}]],
              nvimWebDevicons = [[${p.nvimWebDevicons}]],
              nui = [[${p.nui}]],
              luaEvents = [[${p.luaEvents}]],
              hybridNvim = [[${p.hybridNvim}]],
              neodev = [[${p.neodev}]],
              schemastore = [[${p.schemastore}]],
              mason = [[${p.mason}]],
              masonLspconfig = [[${p.masonLspconfig}]],
              masonNullLs = [[${p.masonNullLs}]],
              noneLs = [[${p.noneLs}]],
              noneLsExtras = [[${p.noneLsExtras}]],
              nvimTsContextCommentstring = [[${p.nvimTsContextCommentstring}]],
              commentNvim = [[${p.commentNvim}]],
              luasnip = [[${p.luasnip}]],
              cmpNvimLsp = [[${p.cmpNvimLsp}]],
              cmpPath = [[${p.cmpPath}]],
              cmpBuffer = [[${p.cmpBuffer}]],
              cmpLuasnip = [[${p.cmpLuasnip}]],
              nvimCmp = [[${p.nvimCmp}]],
              nvimTreesitterRefactor = [[${p.nvimTreesitterRefactor}]],
              nvimTsAutotag = [[${p.nvimTsAutotag}]],
              nvimTreesitter = [[${p.nvimTreesitter}]],
              treesitterContext = [[${p.treesitterContext}]],
              playground = [[${p.playground}]],
              lspSignature = [[${p.lspSignature}]],
              nvimLspconfig = [[${p.nvimLspconfig}]],
              persistence = [[${p.persistence}]],
              neovimSessionManager = [[${p.neovimSessionManager}]],
              neovimProject = [[${p.neovimProject}]],
              fzfLua = [[${p.fzfLua}]],
              telescopeFzfNative = [[${p.telescopeFzfNative}]],
              telescope = [[${p.telescope}]],
              telescopeOrgmode = [[${p.telescopeOrgmode}]],
              chaosNvim = [[${p.chaosNvim}]],
              vimFugitive = [[${p.vimFugitive}]],
              gitsigns = [[${p.gitsigns}]],
              overseer = [[${p.overseer}]],
              nvimDap = [[${p.nvimDap}]],
              nvimDapVirtualText = [[${p.nvimDapVirtualText}]],
              nvimDapUi = [[${p.nvimDapUi}]],
              dapVscodeJs = [[${p.dapVscodeJs}]],
              nvimSurround = [[${p.nvimSurround}]],
              harpoon = [[${p.harpoon}]],
              mongoNvim = [[${p.mongoNvim}]],
              neoTree = [[${p.neoTree}]],
              oil = [[${p.oil}]],
              orgBullets = [[${p.orgBullets}]],
              orgmode = [[${p.orgmode}]],
              pomodoro = [[${p.pomodoro}]],
              presenting = [[${p.presenting}]],
              tmuxNvim = [[${p.tmuxNvim}]],
              vimAbolish = [[${p.vimAbolish}]],
              vimGhost = [[${p.vimGhost}]],
              vimTableMode = [[${p.vimTableMode}]],
            }
          '';

          # Tools made available on nvim's PATH inside the wrapper.
          # These replace mason-tool-installer: all LSP servers, formatters,
          # linters, and debug adapters are pinned here instead of downloaded
          # at runtime.  perlnavigator is absent — see missing.txt.
          extraTools = [
            # build tooling (telescope-fzf-native, etc.)
            pkgs.cmake
            pkgs.ripgrep
            pkgs.fd

            # LSP servers
            pkgs.bash-language-server          # bashls
            pkgs.clang-tools                   # clangd + clang-format
            pkgs.dockerfile-language-server-nodejs # dockerls
            pkgs.vscode-langservers-extracted   # html, jsonls
            pkgs.yaml-language-server           # yamlls
            pkgs.gopls                          # gopls
            pkgs.typescript-language-server     # ts_ls
            pkgs.pyright                        # pyright
            pkgs.lua-language-server            # lua_ls
            pkgs.ansible-language-server        # ansiblels

            # formatters
            pkgs.prettier                       # prettier
            pkgs.black                          # black
            pkgs.shfmt                          # shfmt
            pkgs.biome                          # biome (also a linter)

            # linters
            pkgs.shellcheck                     # shellcheck
            pkgs.ansible-lint                   # ansible-lint

            # debug adapters
            pkgs.delve                          # delve (Go DAP)
          ];

          # ------------------------------------------------------------------
          # Wrapped Neovim
          # ------------------------------------------------------------------
          neovim = pkgs.wrapNeovim pkgs.neovim-unwrapped {
            # Prepend cmake / rg / fd to the PATH that nvim sees, so plugins
            # like telescope-fzf-native (cmake build), telescope live_grep (rg),
            # and find_files (fd) all work without requiring them globally.
            wrapperArgs = [
              "--prefix" "PATH" ":"
              (pkgs.lib.makeBinPath extraTools)
            ];

            configure = {
              # customRC runs via --cmd before the user's init.lua, so lazy.nvim
              # is on runtimepath before plugins.lua calls require("lazy").
              customRC = ''
                " Map of Nix store paths (read by debdut/plugins.lua)
                let $NIX_NVIM_PLUGIN_DIRS = "${pluginDirsLua}"

                " Path to vscode-js-debug for nvim-dap-vscode-js
                let g:vscode_js_debug_path = "${pkgs.vscode-js-debug}/lib/node_modules/js-debug"

                " wrapRc=true sets VIMINIT to this file, bypassing the user's
                " init.lua entirely — so we chain into it explicitly here.
                lua dofile(vim.fn.stdpath('config') .. '/init.lua')
              '';
              packages = { };
            };
          };
        in
        {
          default = neovim;
          neovim  = neovim;
        }
      );

      # Allow `nix run` to launch neovim directly
      apps = forEachSystem (system: {
        default = {
          type    = "app";
          program = "${self.packages.${system}.neovim}/bin/nvim";
        };
      });
    };
}
