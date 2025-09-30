> [!NOTE]
> All of my GitHub repositories have been **archived** and will be migrated to
> Codeberg as I next work on them. This repository either now lives, or will
> live, at:
>
> https://codeberg.org/pbrisbin/tee-io
>
> If you need to report an Issue or raise a PR, and this migration hasn't
> happened yet, send an email to me@pbrisbin.com.

# tee.io

It's like `tee(1)` as a service.

For details, see the [home page](https://tee-io.onrender.com/)

## Development & Test

1. Start the backing services (PostgreSQL & Fake-S3)

   ```
   docker-compose up -d
   ```

1. Set up stack

   ```
   make setup-app
   ```

1. Create development and test databases

   ```
   make setup-db
   ```

1. Develop normally

   ```
   stack test
   stack repl --ghci-options="-DDEVELOPMENT -O0 -fobject-code"
   ```
