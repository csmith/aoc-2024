FROM haskell

RUN cabal update \
    && cabal install --lib regex-tdfa \
