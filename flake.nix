{
  description = "maximilian-huber.de";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = inputs@{ self, nixpkgs, ... }:let
    system = "x86_64-linux";

    pkgs = nixpkgs.legacyPackages.${system};
    lib = pkgs.lib;

    t = lib.trivial;
    hl = pkgs.haskell.lib;

    extraLibraries = with pkgs; [
      zlib
    ];
    project = devTools:
      let addBuildTools = (t.flip hl.addBuildTools) devTools;
          addExtraLibraries = (t.flip hl.addExtraLibraries) extraLibraries;
      in pkgs.haskellPackages.developPackage {
        root = ./.;
        name = "maximilian-huber";
        returnShellEnv = !(devTools == [ ]);

        modifier = (t.flip t.pipe) [
          addBuildTools
          addExtraLibraries
          hl.dontHaddock
          hl.enableStaticLibraries
          hl.justStaticExecutables
          hl.disableLibraryProfiling
          hl.disableExecutableProfiling
          ((t.flip hl.appendBuildFlags) ["--ghc-options=\" -threaded -rtsopts -with-rtsopts=-N\"" "+RTS"])
        ];
      };

  in {

    packages.${system} = {
      default = project [];
      update-script = pkgs.writeShellScriptBin "update.sh" ''
set -euo pipefail

umask 022

ROOT="$( pwd )"
cd $ROOT
INPUT="$ROOT/_site/"
MIRROR="$ROOT/_site-ftp-mirror/"
TARGET="/media/ftp/maximilian-huber.de"

if [[ ! -d "$TARGET" ]]; then
    ~/bin/mountFTP.sh
fi

~/MINE/Bilder/00-galerie/updateFiles.sh "$ROOT/galerie"
${self.packages.${system}.default}/bin/update

################################################################################
# update Files

echo "update files ..."

if [[ ! -d "$INPUT" ]]; then
    exit 1
fi

mkdir -p "$MIRROR"

# make dirs
mydirs=$(cd "$INPUT"; find . \( ! -regex '.*/\..*' \) -type d | sort)
for item in $mydirs ; do
    if [[ -d "$MIRROR/$item" ]]; then
        continue
    fi
    mkdir -p "$TARGET/$item" || true
    mkdir -p "$MIRROR/$item"
done

# copy files
myfiles=$(cd "$INPUT"; find . \( ! -regex '.*/\..*' \) -type f | sort -r)
for item in $myfiles; do
    if [ -f "$MIRROR$item" ]; then
        a=$(md5sum "$INPUT/$item" | awk '{print $1}')
        b=$(md5sum "$MIRROR/$item" | awk '{print $1}')
        if [ "$b" == "$a" ] ; then
            continue
        fi
    fi
    cp -vf "$INPUT/$item" "$TARGET/$item" || true
    cp     "$INPUT/$item" "$MIRROR/$item"
done

# remove outdated
mirrorfiles=$(cd "$MIRROR"; find . \( ! -regex '.*/\..*' \) -type f)
for item in $mirrorfiles; do
    if [[ ! -f "$INPUT/$item" ]]; then
        rm -v "$TARGET/$item" || true
        rm    "$MIRROR/$item"
    fi
done

fusermount -u /media/ftp

        '';
    };

    apps.${system} = {
      update = {
        type = "app";
        program = "${self.packages.${system}.update-script}/bin/update.sh";
      };
    };

    devShell.${system} = project (with pkgs.haskellPackages; [
      cabal-fmt
      cabal-install
      haskell-language-server
      hlint
      ghcid
    ]);

  };
}
