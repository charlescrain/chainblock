.PHONY: start test test-db test-integration test-server

start:
	ENV=Development \
		PORT=3000 \
		PG_HOST=localhost \
		PG_PORT=5432 \
		PG_DBNAME=chainblock \
		PG_USER=postgres \
		PG_PASSWORD=password \
		stack exec tholos

test:
	ENV=Test \
		PORT=3000 \
		PG_HOST=localhost \
		PG_PORT=5432 \
		PG_DBNAME=chainblocktest \
		PG_USER=postgres \
		PG_PASSWORD=password \
		stack test tholos

test-db:
	ENV=Test \
		PORT=3000 \
		PG_HOST=localhost \
		PG_PORT=5432 \
		PG_DBNAME=chainblocktest \
		PG_USER=postgres \
		PG_PASSWORD=password \
		stack test tholos --test-arguments="--match=Data"

test-server:
	ENV=Test \
		PORT=3000 \
		PG_HOST=localhost \
		PG_PORT=5432 \
		PG_DBNAME=chainblocktest \
		PG_USER=postgres \
		PG_PASSWORD=password \
		stack test tholos --test-arguments="--match=Server"

test-integration:
	ENV=Test \
		PORT=3000 \
		PG_HOST=localhost \
		PG_PORT=5432 \
		PG_DBNAME=chainblocktest \
		PG_USER=postgres \
		PG_PASSWORD=password \
		stack test tholos --test-arguments="--match=Integration"
