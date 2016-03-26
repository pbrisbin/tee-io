.PHONY: test

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
