{
  description = "Heph - data structures & algorithms for Haskell games and renderers";

  inputs = {
    haskellNix.url = "github:ican-group/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    haskellNix,
    ...
  }: let
    supportedSystems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin"];

    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

    mkOutputs = system: let
      ghcVersion = "ghc912";

      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [haskellNix.overlay];
      };

      tools = with pkgs; {
        build = with llvmPackages_22; [clang llvm];
        check = [haskellPackages.cabal-fmt lychee oxfmt shellcheck typos];
        workflow = [
          ast-grep
          bat
          coreutils
          dasel
          diffutils
          dyff
          fd
          gawk
          git
          jq
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
        src = ./.;
        compiler-nix-name = ghcVersion;
        supportHpack = true;

        shell = {
          withHoogle = true;
          tools = {
            cabal = "latest";
            fourmolu = "latest";
            haskell-language-server = "latest";
            hlint = "latest";
            hoogle = "latest";
            hpack = "latest";
            tasty-discover = "latest";
          };
          nativeBuildInputs = tools.build ++ tools.check;
          buildInputs = tools.workflow;
        };
      };

      # Generate the base flake outputs from haskell.nix
      flake = project.flake {};
    in {
      inherit (flake) apps;

      inherit (flake) devShells;

      formatter = pkgs.alejandra;

      packages =
        flake.packages
        // {
          default = flake.packages."heph-sparse-set:lib:heph-sparse-set";
        };

      checks =
        flake.checks
        // {
          fourmolu =
            pkgs.runCommand "fourmolu-check" {
              nativeBuildInputs = [(project.tool "fourmolu" "latest")];
            } ''
              cd ${./.}
              fourmolu -m check .
              touch $out
            '';

          hlint =
            pkgs.runCommand "hlint-check" {
              nativeBuildInputs = [(project.tool "hlint" "latest")];
            } ''
              cd ${./.}
              hlint --ignore-suggestions .
              touch $out
            '';

          shellcheck =
            pkgs.runCommand "shellcheck-check" {
              nativeBuildInputs = [pkgs.shellcheck];
            } ''
              cd ${./.}
              find . -type f -name "*.sh" -exec shellcheck {} +
              touch $out
            '';

          typos =
            pkgs.runCommand "typos-check" {
              nativeBuildInputs = [pkgs.typos];
            } ''
              cd ${./.}
              typos
              touch $out
            '';

          # Local files and anchors only: the sandbox has no network. Links into
          # this repo's main branch are checked against the source tree.
          links =
            pkgs.runCommand "links-check" {
              nativeBuildInputs = [pkgs.lychee];
              # lychee builds its HTTP client even with --offline
              SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
            } ''
              cd ${./.}
              lychee --offline --no-progress \
                --remap "^https://(?:github\.com/jtnuttall/heph/(?:blob|tree)|raw\.githubusercontent\.com/jtnuttall/heph)/main/([^#?]*) file://$PWD/\$1" \
                .
              touch $out
            '';

          oxfmt =
            pkgs.runCommand "oxfmt-check" {
              nativeBuildInputs = [pkgs.oxfmt];
            } ''
              cd ${./.}
              oxfmt --check
              touch $out
            '';
        };
    };
  in {
    packages = forAllSystems (sys: (mkOutputs sys).packages);
    apps = forAllSystems (sys: (mkOutputs sys).apps);
    devShells = forAllSystems (sys: (mkOutputs sys).devShells);
    checks = forAllSystems (sys: (mkOutputs sys).checks);
    formatter = forAllSystems (sys: (mkOutputs sys).formatter);
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.zw3rk.com"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
