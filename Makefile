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

build:
	docker build \
	  --tag pbrisbin/tee-io-build \
	  --file docker/Dockerfile.build .

binaries:
	docker run --rm \
	  --volume "$(PWD)":/src:ro \
	  --volume "$(PWD)"/docker/bin:/root/.local/bin \
	  --volume "$(PWD)"/docker/stack:/root/.stack \
	  --volume "$(PWD)"/docker/stack-work:/src/.stack-work \
	  pbrisbin/tee-io-build sh -c "stack setup && stack install"

production:
	docker build \
	  --tag pbrisbin/tee-io \
	  --file docker/Dockerfile .

release:
	docker tag pbrisbin/tee-io registry.heroku.com/tee-io/web
	docker push registry.heroku.com/tee-io/web

repl:
	stack repl --ghci-options="-DDEVELOPMENT -O0 -fobject-code"
