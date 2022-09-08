# Build stage
FROM fpco/stack-build-small:lts-13.17 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    libpq-dev \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

ENV PATH /root/.local/bin:$PATH

RUN mkdir -p /src
WORKDIR /src

COPY stack.yaml /src/
RUN stack setup

COPY tee-io.cabal /src/tee-io.cabal
RUN stack install --dependencies-only

COPY src /src/src
COPY app /src/app
COPY config /src/config
COPY static /src/static
COPY templates /src/templates
RUN stack install

# Runtime
FROM ubuntu:18.04
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    gcc \
    libpq-dev \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

RUN mkdir -p /app
WORKDIR /app
COPY config /app/config
COPY static /app/static
COPY --from=builder /root/.local/bin/tee-io /app/tee-io
COPY --from=builder /root/.local/bin/tee-io-worker /app/tee-io-worker
COPY --from=builder /lib/x86_64-linux-gnu/libm.so.6 /lib/x86_64-linux-gnu/libm.so.6

CMD ["/app/tee-io"]
