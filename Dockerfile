FROM elmanhasa/aarch64-haskell-base:cached
COPY . /home/app/haskell-chain
USER root
RUN apk --update add leveldb
RUN chown -R app /home/app/haskell-chain
USER app
WORKDIR /home/app/haskell-chain

RUN nix-build release.nix

CMD ["/home/app/haskell-chain/result/bin/haskell-chain-exe"]