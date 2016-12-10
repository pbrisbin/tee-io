.PHONY: setup-db setup-app repl devel worker test production release

setup-db:
	docker-compose up -d postgres && sleep 10
	docker-compose exec --user postgres postgres sh -c \
	  "createdb teeio && createdb teeio_test"

setup-app:
	docker-compose run --rm tee-io sh -c "\
	  stack setup &&\
	  stack build --dependencies-only --test &&\
	  stack install yesod-bin"

setup: setup-db setup-app

repl:
	docker-compose run --rm tee-io \
	  stack repl --ghc-options="-DDEVELOPMENT -O0 -fobject-code"

devel:
	docker-compose run --rm tee-io yesod devel

worker:
	docker-compose run --rm tee-io sh -c \
	  "stack build && stack exec tee-io-worker"

test:
	docker-compose run --rm \
	  -e DATABASE_URL=postgres://postgres@postgres:5432/teeio_test \
	  -e LOG_LEVEL=error \
	  -e S3_URL=http://fake-s3:4569/tee.io_test \
	  tee-io stack test

production:
	docker-compose run --rm tee-io sh -c "\
	  stack install &&
	  cp -v ~/.local/bin/tee-io* out/"
	docker build --tag pbrisbin/tee-io .

release:
	docker tag pbrisbin/tee-io registry.heroku.com/tee-io/web
	docker push registry.heroku.com/tee-io/web
