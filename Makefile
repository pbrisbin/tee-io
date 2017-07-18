.PHONY: setup-db setup-app test image release repl

setup-db:
	docker-compose exec --user postgres postgres sh -c \
	  "createdb teeio && createdb teeio_test"

setup-app:
	stack setup
	stack build --fast
	stack build --fast --test --no-run-tests

setup-ci:
	createdb teeio_test
	docker run --detach --publish 4569:4569 lphoward/fake-s3
	mv .env.ci .env.test

test:
	stack test

image:
	docker-compose build tee-io

release:
	docker tag pbrisbin/tee-io registry.heroku.com/tee-io/web
	docker push registry.heroku.com/tee-io/web

repl:
	stack repl --ghci-options="-DDEVELOPMENT -O0 -fobject-code"
