set shell := ["bash", "-euo", "pipefail", "-c"]

system := `nix eval --raw --impure --expr builtins.currentSystem 2>/dev/null || true`
ifd := "--option allow-import-from-derivation true"

default:
    @just --list

# == nix ==

check *args:
    nix-fast-build --flake '.#checks.{{system}}' {{ifd}} {{args}}

check-one name:
    nix-fast-build --flake '.#checks.{{system}}.{{name}}' {{ifd}}

# == formatting ==

fmt *args:
    treefmt {{args}}

fmt-check:
    treefmt --fail-on-change --no-cache

# == cabal-side loops (the nix checks run these hermetically) ==

test *targets="all":
    cabal test {{targets}}

haddock *targets="all":
    cabal haddock {{targets}}

links:
    lychee '**/*.md' '**/*.cabal' '**/package.yaml' '**/*.hs' '**/*.nix' '.github/**/*.yml'

# == hpack ==

hpack:
    for f in */package.yaml; do \
      (cd "$(dirname "$f")" && hpack) \
    done

hpack-check:
    for f in */package.yaml; do \
      dir=$(dirname "$f"); \
      cabal_file=$(ls "$dir"/*.cabal); \
      (cd "$dir" && hpack - | diff -u "$(basename "$cabal_file")" -) \
        || { echo "$cabal_file is stale; run 'just hpack'"; exit 1; }; \
    done
