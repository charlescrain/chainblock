.PHONY: build test citest

start-chainblock:
	ENV=Development \
		PORT=3000 \
		DB_NAME=webapp \
		DB_USER=postgres \
		DB_PASS=webapp \
		DB_HOST=172.19.0.1 \
		DB_POOL_SIZE=5 \
		stack exec webapp
