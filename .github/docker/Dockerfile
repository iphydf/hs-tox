FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-tox"]
COPY --chown=builder:users . /work/hs-tox
RUN ["stack", "install", "tox"]

FROM scratch
COPY --from=build /home/builder/.local/ /
