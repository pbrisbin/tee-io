# tee.io Ruby SDK

Ruby client SDK for [tee.io][].

[tee.io]: https://tee-io.herokuapp.com

## Installation

```
gem "tee-io", require: "tee_io"
```

## Usage

```rb
TeeIO.run("cap deploy") { |url| puts url }
```

The following keyword options are supported:

- `description`: supply a description, defaults to the command
- `timeout`: timeout in seconds, defaults to `5 * 60`
- `base_url`: override the base URL (useful for local testing)
