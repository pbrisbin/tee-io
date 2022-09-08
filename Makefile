.PHONY: setup-db setup-app test build binaries production release repl

setup-db:
	docker-compose exec --user postgres postgres sh -c \
	  "createdb teeio && createdb teeio_test"

setup-app:
	stack setup
	stack build --fast
	stack build --fast --test --no-run-tests

setup-ci:
	sed -i 's/postgres:postgres@//' .env.test
	mkdir -p docker/stack docker/stack-work
	sudo chown root:root docker/stack docker/stack-work
	createdb teeio_test
	docker run --detach --publish 4569:4569 lphoward/fake-s3

test:
	stack test

repl:
	stack repl --ghci-options="-DDEVELOPMENT -O0 -fobject-code"
