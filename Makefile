.PHONY: start test test-db

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

test-business:
	ENV=Test \
		stack test tholos --test-arguments="--match=Business"
