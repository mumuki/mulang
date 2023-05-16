WORKDIR /root/mulang

COPY apt-transport-https_1.0.9.8.6_amd64.deb apt-transport-https_1.0.9.8.6_amd64.deb
COPY node-v8.17.0-linux-x64.tar.xz node-v8.17.0-linux-x64.tar.xz
tar xf node-v8.17.0-linux-x64.tar.xz

RUN dpkg -i apt-transport-https_1.0.9.8.6_amd64.deb
RUN stack upgrade --binary-version 1.9.3 \
    && stack --no-terminal --skip-ghc-check --allow-different-user test

RUN rm /root/.ghcjs/x86_64-linux-0.2.1.9007019-8.0.1/package.conf.d/ -rf
RUN rm -rf /root/.ghcjs/x86_64-linux-0.2.1.9007019-8.0.1/package.conf.d

RUN export PATH=/root/.local/bin:$PATH
RUN export PATH="$PATH:/root/node-v18.14.0-linux-x64/bin/"

RUN ./ghcjslib/swap.sh
RUN ./ghcjslib/test.sh
RUN export PATH="$PATH:/root/node-v8.17.0-linux-x64/bin/"
RUN export PATH="/root/.local/bin:/root/.cabal/bin:/root/.local/bin:/opt/cabal/1.24/bin:/opt/ghc/8.0.1/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/node-v18.14.0-linux-x64/bin/"
RUN export PATH="/root/.local/bin:/root/.cabal/bin:/root/.local/bin:/opt/cabal/1.24/bin:/opt/ghc/8.0.1/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin"
RUN export PATH="$PATH:/root/node-v8.17.0-linux-x64/bin/"
RUN export PATH="/root/.local/bin:/root/.cabal/bin:/root/.local/bin:/opt/cabal/1.24/bin:/opt/ghc/8.0.1/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/node-v8.17.0-linux-x64/bin/"