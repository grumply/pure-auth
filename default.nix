{ mkDerivation, stdenv, ghc, base, bytestring, pure-txt, pure-json, pure-maybe, pure-elm, 
  pure-localstorage, pure-websocket, pure-sorcerer, hashable, crypto-api, pwstore-fast,
  pure-hooks
}:
mkDerivation {
  pname = "pure-auth";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    pure-elm 
    pure-txt
    pure-json
    pure-maybe
    pure-localstorage
    pure-websocket
    pure-hooks
    hashable
  ] ++ (if ghc.isGhcjs or false
        then [ ]
        else [ bytestring pure-sorcerer crypto-api pwstore-fast ]
    );
  homepage = "github.com/grumply/pure-auth";
  license = stdenv.lib.licenses.bsd3;
}
