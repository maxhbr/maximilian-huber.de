{ mkDerivation, base, blaze-html, blaze-markup, clay, directory
, filepath, markdown, split, stdenv, text, unix
}:
mkDerivation {
  pname = "maximilian-huber";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base blaze-html blaze-markup clay directory filepath markdown split
    text unix
  ];
  homepage = "maximilian-huber.de";
  description = "my homepage";
  license = stdenv.lib.licenses.bsd3;
}
