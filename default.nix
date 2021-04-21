with import <nixpkgs> { };

runCommand "lam" {
  src = ./.;
  buildInputs = [
    jdk
    (haskellPackages.ghcWithPackages (p: [
      p.language-java
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
