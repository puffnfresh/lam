with import <nixpkgs> { };

runCommand "lam" {
  src = ./.;
  buildInputs = [
    jdk8_headless
    (haskellPackages.ghcWithPackages (p: [
      (haskell.lib.appendPatch p.language-java (fetchpatch {
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
  export LOCALE_ARCHIVE=${glibcLocales}/lib/locale/locale-archive
  runhaskell Gen.hs
  mv build/lam.jar $out
"
