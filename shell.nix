let 
  rien = import .rien/rien.nix {
    packageName = "pico";
    packagePath = ./.;

    # Instead of using <nixpkgs>, use a lock-file to stick to
    # a particular `nixpkgs` commit.
    nixpkgsLock = ./nixpkgs.json;

    ghcVersion = "ghc822";

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
      ghc-mod
      cabal-helper
      hpack
      ghcid
      hlint
      stylish-haskell
      JuicyPixels
      errors
      temporary
      directory
      process
      rio
      hashable
      hint

      bifunctors
      distributive
      free
      kan-extensions
      comonad
      profunctors
      adjunctions
      semigroupoids
      semigroups
      void
      contravariant
      pointed
      tagged
      constraints
      reflection

      pandoc-types
      pandoc
      lens
      lens-aeson
      foldl
      streaming
      managed
      stm
      stm-containers
      streaming-utils
      streaming-bytestring

      clay
      filepath
      type-of-html
      text
      hinotify
      fsnotify
      binary
      mmark
      skylighting
      mmark-ext
      unordered-containers
      stache
      htoml-megaparsec
      toml-parser
      lens-toml-parser
      shake
      shake-path
      path
      protolude

      wai
      wai-app-static
      aeson
      warp
      directory

      # superrecord
      vinyl
      vinyl-utils
      singletons

      blaze-html
      time
      blaze-markup

      servant
      servant-server
      servant-generic

      aeson
      aeson-compat
      attoparsec
      base-compat
      bytestring
      containers
      directory
      http-api-data
      http-client
      http-media
      http-types
      mtl
      string-conversions
      text
      transformers
      wai
      lucid

      esqueleto
      persistent
      sqlite-simple
      postgresql-simple
      selda
      selda-sqlite
      selda-postgresql

      ghc-proofs
    ];

    # Optionally, also add sets of related packages that are
    # commonly used together.
    depSets = hsPkgs: with (rien.package-sets hsPkgs); [
      development-servers
    ];

    # Native dependencies
    nativeDeps = pkgs: with pkgs; [ 
      # z3 
      # llvm_5
    ];
  }) // { shellHook = "source setup-ghcmod.sh"; } 
