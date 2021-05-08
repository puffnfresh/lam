let
  pkgs =
    import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz) { };
in
pkgs.runCommand "lam" {
  src = ./.;
  buildInputs = [
    pkgs.jdk8_headless
    (pkgs.haskellPackages.ghcWithPackages (p: [
      (pkgs.haskell.lib.appendPatch p.language-java (pkgs.fetchpatch {
        url = "https://github.com/vincenthz/language-java/commit/4a52a781ddb9c66b8b66d0960e1390eaa74e5e49.patch";
        sha256 = "0bcf48f9riyy9r09c6fccqjl817paxc6zwiwmhzykira7kc71zrq";
      }))
      p.lens
      p.shake
    ]))
  ];
} "
  cp -r $src src
  chmod -R u+w src
  cd src
  export LANG=en_US.UTF-8
  export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
  runhaskell Gen.hs
  mv build/lam.jar $out
"
