let 
  rien = import .rien/rien.nix {
    packageName = "pico";
    packagePath = ./.;

    # Instead of using <nixpkgs>, use a lock-file to stick to
    # a particular `nixpkgs` commit.
    nixpkgsLock = ./nixpkgs.json;

    ghcVersion = "ghc865";

    overrides = rec {
      jailbreak = [ "cabal-helper" "ghc-mod" "liquidhaskell" "streaming-utils" ];
      skipHaddock = justStaticExecutables;
      skipTests = [ "cabal-helper" "ghc-mod" ];
      justStaticExecutables = [ 
        "brittany" 
        "hpack"
        "ghcid"
      ];
    };
  };

in
  (rien.shell {
    # Generate Hoogle documentation?
    wantHoogle = true;

    # Haskell dependencies
    deps = hsPkgs: with hsPkgs; [
      brittany
      hpack
      ghcid
      hlint
      stylish-haskell

      mtl
      constraints
      protolude

      array
      base
      base-orphans
      bifunctors
      bytestring
      call-stack
      comonad
      containers
      contravariant
      distributive
      exceptions
      filepath
      free
      generic-deriving
      ghc-prim
      hashable
      kan-extensions
      mtl
      nats
      parallel
      profunctors
      reflection
      semigroupoids
      semigroups
      tagged
      template-haskell
      text
      th-abstraction
      transformers
      unordered-containers
      vector
      void

    ];

    # Optionally, also add sets of related packages that are
    # commonly used together.
    depSets = hsPkgs: with (rien.package-sets hsPkgs); [
    ];

    # Native dependencies
    nativeDeps = pkgs: with pkgs; [ 
      # z3 
      # llvm_5
    ];
  })
