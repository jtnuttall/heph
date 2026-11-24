{hsTools, ...}: {
  projectRootFile = "flake.nix";

  settings.excludes = [
    # Written by nix; oxfmt would fight it.
    "flake.lock"
  ];

  programs = {
    alejandra.enable = true;
    deadnix.enable = true;
    fourmolu = {
      enable = true;
      package = hsTools.fourmolu;
    };
    hlint = {
      enable = true;
      package = hsTools.hlint;
    };
    oxfmt.enable = true;
    shellcheck.enable = true;
    shfmt = {
      enable = true;
      indent_size = 2;
      simplify = true;
    };
    statix.enable = true;
    typos = {
      enable = true;
      # Report only: typos' corrections are not always right.
      autoFix = false;
    };
  };

  settings.programs.fourmolu = {
    ghcOpts = [];
  };

  # hlint reads .hlint.yaml from the project root; keep the pre-treefmt
  # behaviour of failing on warnings and errors only.
  settings.formatter.hlint.options = ["--ignore-suggestions"];
}
