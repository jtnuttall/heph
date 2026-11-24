{
  description = "Heph - data structures & algorithms for Haskell games and renderers";

  inputs = {
    haskellNix.url = "github:ican-group/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    treefmt-nix.url = "github:numtide/treefmt-nix/a0a97e31a897a3537eea5c49df4f8adaa83b3b37";
    treefmt-nix.inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    haskellNix,
    flake-utils,
    treefmt-nix,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "aarch64-darwin"]
    (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [haskellNix.overlay];
        };

        src = ./.;

        # The primary compiler. `nix flake check` also runs the project's test
        # suites under the variants below; the devShell is the primary only.
        ghcVersion = "ghc912";
        ghcVariants = {
          ghc9103.compiler-nix-name = pkgs.lib.mkForce "ghc9103";
          ghc9141.compiler-nix-name = pkgs.lib.mkForce "ghc9141";
        };

        hsToolsSpec = {
          cabal = "latest";
          fourmolu = "latest";
          haskell-language-server = "latest";
          hlint = "latest";
          hoogle = "latest";
          hpack = "latest";
          tasty-discover = "latest";
        };

        tools = with pkgs; {
          build = with llvmPackages_22; [clang llvm];
          check = [haskellPackages.cabal-fmt lychee oxfmt shellcheck typos treefmtWrapper];
          workflow = [
            ast-grep
            bat
            glslang
            shaderc
            spirv-tools
            coreutils
            dasel
            diffutils
            dyff
            fd
            gawk
            git
            jq
            just
            nix-fast-build
            # pyparsing: Vulkan-Docs' genvk.py generates the exactly-pinned
            # vulkan_core.h for lithon-codegen's ABI static-assert gate
            (python314.withPackages (ps: [ps.pyparsing]))
            ripgrep
            scc
            tree-sitter
            xmlstarlet
            xq
          ];
        };

        project = pkgs.haskell-nix.cabalProject' {
          inherit src;
          compiler-nix-name = ghcVersion;
          supportHpack = true;

          shell = {
            withHoogle = true;
            tools = hsToolsSpec;
            nativeBuildInputs = tools.build ++ tools.check;
            buildInputs = tools.workflow;
          };
        };

        hsTools = project.tools hsToolsSpec;

        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          imports = [./nix/treefmt.nix];
          _module.args.hsTools = hsTools;
        };

        treefmtWrapper = treefmtEval.config.build.wrapper;

        extraChecks =
          {
            formatting = treefmtEval.config.build.check self;
          }
          // import ./nix/checks.nix {
            inherit pkgs;
            inherit src;
            inherit project;
            inherit hsTools;
          };

        flake = removeAttrs (project.flake {variants = ghcVariants;}) ["hydraJobs"];
      in
        flake
        // {
          formatter = treefmtWrapper;

          inherit (flake) apps devShells;

          packages =
            flake.packages
            // {
              default = flake.packages."heph-sparse-set:lib:heph-sparse-set";
            };

          checks = flake.checks // extraChecks;
        }
    );
}
