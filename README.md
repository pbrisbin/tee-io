# tee.io

It's like `tee(1)` as a service.

For details, see the [home page](https://tee-io.herokuapp.com/)

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
