FROM haskell

RUN cabal update \
    && cabal install --lib \
      array \
      containers \
      regex-tdfa \
      split
