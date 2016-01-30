FROM pbrisbin/heroku-haskell-stack:1.0.2
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

COPY src /app/user/src
COPY app /app/user/app
COPY bin /app/user/bin
COPY static /app/user/static
COPY config /app/user/config
COPY templates /app/user/templates
RUN stack install

# Reduce slug size
RUN rm -rf /app/user/.stack-work
