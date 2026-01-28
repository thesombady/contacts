cabal install exe:contacts \
  --installdir=$HOME/.lib/bin \
  --overwrite-policy=always \
  --ghc-options="-O2"

mkdir -p $HOME/.contacts/
cp addresses.toml $HOME/.contacts/
