FROM fpco/stack-build:lts AS builder
ENV LANG en_US.UTF-8
RUN mkdir /src
WORKDIR /src

# Old LTS + new Stack fails on Happy hack. Attempted workaround:
RUN stack install alex happy
ENV PATH=/root/.local/bin:$PATH

COPY stack.yaml /src/stack.yaml
COPY tee-io.cabal /src/tee-io.cabal

RUN stack setup
RUN stack build --dependencies-only

COPY src /src/src
COPY app /src/app
COPY config /src/config
COPY static /src/static
COPY templates /src/templates
RUN stack install

FROM fpco/stack-run:lts
ENV LANG en_US.UTF-8
RUN mkdir -p /app
WORKDIR /app

COPY config /app/config
COPY static /app/static
COPY --from=builder /root/.local/bin/tee-io /app/tee-io
COPY --from=builder /root/.local/bin/tee-io-worker /app/tee-io-worker
COPY --from=builder /lib/x86_64-linux-gnu/libm.so.6 /lib/x86_64-linux-gnu/libm.so.6

CMD ["/app/tee-io"]
