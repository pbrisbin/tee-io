FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8

RUN mkdir -p /app
WORKDIR /app

# Executables
COPY docker/bin/tee-io /app/tee-io
COPY docker/bin/tee-io-worker /app/tee-io-worker

# Support resources
COPY config /app/config
COPY static /app/static

RUN useradd app
USER app

# Reset fpco/stack-run's dumb ENTRYPOINT
ENTRYPOINT []
CMD ["/app/tee-io"]
