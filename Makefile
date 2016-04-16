.PHONY: setup test

setup:
	createdb teeio
	createdb teeio_test
	echo \
	  "CREATE USER teeio WITH PASSWORD 'teeio';" \
	  " GRANT ALL PRIVILEGES ON DATABASE teeio TO teeio; " \
	  " GRANT ALL PRIVILEGES ON DATABASE teeio_test TO teeio;" |\
	  psql template1
	stack setup
	stack build --dependencies-only --test

test:
	@docker stop tee-io-fake-s3 || true
	@docker rm tee-io-fake-s3 || true
	docker run \
	  --detach \
	  --name tee-io-fake-s3 \
	  --publish 4569:4569 \
	  lphoward/fake-s3
	stack test
	@docker stop tee-io-fake-s3 || true
	@docker rm tee-io-fake-s3 || true
