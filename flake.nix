{
  description = "maximilian-huber.de";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.sync = pkgs.writeShellScriptBin "sync-ftp" ''
        export PATH="${pkgs.lib.makeBinPath [ pkgs.lftp (pkgs.pass.withExtensions (exts: [ ])) pkgs.coreutils pkgs.gnused pkgs.gnugrep ]}:$PATH"
        exec ${pkgs.bash}/bin/bash ${./sync-ftp..sh} "$@"
      '';

      packages.${system}.gftp = pkgs.writeShellScriptBin "gftp" ''
        export PATH="${pkgs.lib.makeBinPath [ pkgs.gftp (pkgs.pass.withExtensions (exts: [ ])) pkgs.coreutils pkgs.gnused pkgs.gnugrep ]}:$PATH"
        exec ${pkgs.bash}/bin/bash ${./gftp.sh} "$@"
      '';

      apps.${system}.sync = {
        type = "app";
        program = "${self.packages.${system}.sync}/bin/sync-ftp";
      };

      apps.${system}.gftp = {
        type = "app";
        program = "${self.packages.${system}.gftp}/bin/gftp";
      };
    };
}
