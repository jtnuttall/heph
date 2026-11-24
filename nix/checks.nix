{
  pkgs,
  project,
  src,
  hsTools,
}: let
  haddock = pkg: project.hsPkgs.${pkg}.components.library.doc;
in {
  check-hpack =
    pkgs.runCommand "check-hpack" {
      nativeBuildInputs = [hsTools.hpack pkgs.diffutils];
    } ''
      for cabal_file in ${src}/*/*.cabal; do
        dir="$(dirname "$cabal_file")"
        cd "$dir"

        echo "Verifying that hpack's output matches $cabal_file..."
        diff_out="$(hpack - | diff -u "$cabal_file" -)" || {
          echo "$diff_out"
          echo "$cabal_file: Expected hpack output differs from actual cabal file."
          echo "Did you forget to run 'hpack' in $dir?"
          exit 1
        }
      done

      touch $out
    '';

  check-haddock-heph-aligned-storable = haddock "heph-aligned-storable";
  check-haddock-heph-aligned-storable-geomancy = haddock "heph-aligned-storable-geomancy";
  check-haddock-heph-aligned-storable-linear = haddock "heph-aligned-storable-linear";
  check-haddock-heph-aligned-storable-testlib = haddock "heph-aligned-storable-testlib";
  check-haddock-heph-sparse-set = haddock "heph-sparse-set";
}
