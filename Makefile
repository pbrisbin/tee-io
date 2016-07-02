.PHONY: setup development test production release repl

setup: development
	createdb teeio
	createdb teeio_test
	echo \
	  "CREATE USER teeio WITH PASSWORD 'teeio';" \
	  " GRANT ALL PRIVILEGES ON DATABASE teeio TO teeio; " \
	  " GRANT ALL PRIVILEGES ON DATABASE teeio_test TO teeio;" |\
	  psql template1
	bin/stack setup
	bin/stack build --dependencies-only --test

development:
	docker build \
	  --tag pbrisbin/tee-io-development \
	  --file docker/Dockerfile.build .

test:
	@docker stop tee-io-fake-s3 || true
	@docker rm tee-io-fake-s3 || true
	docker run \
	  --detach \
	  --name tee-io-fake-s3 \
	  --publish 4569:4569 \
	  lphoward/fake-s3
	bin/stack test
	@docker stop tee-io-fake-s3 || true
	@docker rm tee-io-fake-s3 || true

production:
	bin/stack install
	docker build \
	  --tag pbrisbin/tee-io \
	  --file docker/Dockerfile .

release:
	docker tag pbrisbin/tee-io registry.heroku.com/tee-io/web
	docker push registry.heroku.com/tee-io/web

repl:
	bin/stack repl --ghc-options="-DDEVELOPMENT -O0 -fobject-code"
