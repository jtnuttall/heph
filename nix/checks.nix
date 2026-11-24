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

  # The core and the testlib must not depend on a particular math library;
  # those bindings live in the heph-aligned-storable-{linear,geomancy} packages.
  check-linear-isolation =
    pkgs.runCommand "check-linear-isolation" {
      nativeBuildInputs = [pkgs.gnugrep];
    } ''
      cd ${src}
      if grep -rn "import.*Linear" heph-aligned-storable/src heph-aligned-storable-testlib/src; then
        echo "linear leaked into heph-aligned-storable or heph-aligned-storable-testlib"
        exit 1
      fi
      touch $out
    '';

  # Local files and anchors only: the sandbox has no network. Links into
  # this repo's main branch are checked against the source tree. The CI
  # `links` job covers external URLs.
  check-links-offline =
    pkgs.runCommand "check-links-offline" {
      nativeBuildInputs = [pkgs.lychee];
      # lychee builds its HTTP client even with --offline
      SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    } ''
      cd ${src}
      lychee --offline --no-progress \
        --remap "^https://(?:github\.com/jtnuttall/heph/(?:blob|tree)|raw\.githubusercontent\.com/jtnuttall/heph)/main/([^#?]*) file://$PWD/\$1" \
        .
      touch $out
    '';

  # == haddock ==
  check-haddock-heph-aligned-storable = haddock "heph-aligned-storable";
  check-haddock-heph-aligned-storable-geomancy = haddock "heph-aligned-storable-geomancy";
  check-haddock-heph-aligned-storable-linear = haddock "heph-aligned-storable-linear";
  check-haddock-heph-aligned-storable-testlib = haddock "heph-aligned-storable-testlib";
  check-haddock-heph-sparse-set = haddock "heph-sparse-set";
}
