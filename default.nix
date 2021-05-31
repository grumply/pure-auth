{ mkDerivation, stdenv, ghc, base, bytestring, pure-txt, pure-json, pure-elm, 
  pure-localstorage, pure-websocket, sorcerer, hashable, crypto-api, pwstore-fast
}:
mkDerivation {
  pname = "pure-auth";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    pure-txt
    pure-json
    pure-websocket
    hashable
  ] ++ (if ghc.isGhcjs or false
        then [ pure-elm pure-localstorage ]
        else [ bytestring sorcerer crypto-api pwstore-fast ]
    );
  homepage = "github.com/grumply/pure-auth";
  license = stdenv.lib.licenses.bsd3;
}
