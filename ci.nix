{ version ? builtins.readFile ./version.txt }:

let
  pkgs =
    import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz) {
      overlays = [
        (import "${builtins.fetchTarball "https://github.com/hercules-ci/hercules-ci-effects/archive/b67cfbbb31802389e1fb6a9c75360968d201693b.tar.gz"}/overlay.nix")
      ];
    };
in
rec {
  lam = import ./default.nix;
  pinata = pkgs.effects.mkEffect {
    name = "pinata";
    src = "${lam}/lam-javadoc-0.0.1-SNAPSHOT.jar";
    buildInputs = [ pkgs.unzip ];
    unpackCmd = ''
      unzip -d lam-javadoc -qq $src
    '';
    secretsMap.pinata = "pinata";
    effectScript = ''
      base="lam"

      declare -a curl_params
      for file in $(find . -type f); do
        filename=''${file#"./"}
        curl_params+=("-F" "file=@$file;filename=$base/$filename")
      done

      result=$(
        curl \
          -H "pinata_api_key: $(readSecretString pinata .key)" \
          -H "pinata_secret_api_key: $(readSecretString pinata .secret)" \
          ''${curl_params[@]} \
          https://api.pinata.cloud/pinning/pinFileToIPFS \
      )
      echo "$result" | jq -r '.IpfsHash'
    '';
  };
}
