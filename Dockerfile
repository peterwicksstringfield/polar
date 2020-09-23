FROM debian:buster

RUN apt-get update && apt-get install -y \
    build-essential \
    binutils \
    curl

# We need abunch of stuff to build GHC.
RUN apt-get install -y \
    libffi-dev \
    libffi6 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libnuma1 \
    libnuma-dev \
    libtinfo5 \
    numactl

RUN apt-get install -y \
    hugs

ENV USER builder

RUN useradd -ms /bin/bash $USER

USER $USER
ENV HOME /home/$USER
WORKDIR $HOME
RUN mkdir -p project
WORKDIR $HOME/project/

RUN curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > ghcup

RUN chmod +x ghcup

ENV PATH="/home/builder/.ghcup/bin:$PATH"

RUN ./ghcup install ghc 8.10.2
RUN ./ghcup install ghc 8.8.4
RUN ./ghcup install ghc 8.6.5
RUN ./ghcup install ghc 8.4.4
RUN ./ghcup install ghc 8.2.2
RUN ./ghcup install ghc 8.0.2

RUN ./ghcup set ghc 8.10.2
RUN ./ghcup install cabal

RUN cabal update

RUN cabal v2-install --lib HUnit

COPY --chown=$USER Data/ Data/
COPY --chown=$USER test/ test/
COPY --chown=$USER polar.cabal polar.cabal
COPY --chown=$USER LICENSE LICENSE

RUN ./ghcup set ghc 8.10.2
RUN cabal test

RUN ./ghcup set ghc 8.8.4
RUN cabal test

RUN ./ghcup set ghc 8.6.5
RUN cabal test

RUN ./ghcup set ghc 8.4.4
RUN cabal test

RUN ./ghcup set ghc 8.2.2
RUN cabal test

RUN ./ghcup set ghc 8.0.2
RUN cabal test

RUN cpp -traditional-cpp Data/Complex/Polar.hs | sed "/^#/d" > Data/Complex/Polar.hs.preprocessed
RUN cpp -traditional-cpp test/Main.hs | sed "/^#/d" > test/Main.hs.preprocessed
RUN mv Data/Complex/Polar.hs.preprocessed Data/Complex/Polar.hs
RUN mv test/Main.hs.preprocessed test/Main.hs
RUN runhugs -98 test/Main.hs

ENTRYPOINT ["/bin/bash"]

# To build and run tests with many GHC versions:
# docker build -t polar-testbuild .

# To get into a shell inside the container to debug things:
# docker run -it --rm polar-testbuild