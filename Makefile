.PHONY: start-chainblock test test-db

start-chainblock:
	ENV=Development \
		PORT=3000 \
		PG_HOST=localhost \
		PG_PORT=5432 \
		PG_DBNAME=chainblock \
		PG_USER=postgres \
		PG_PASSWORD=password \
		stack exec chainblock

test:
	ENV=Test \
		PORT=3000 \
		PG_HOST=localhost \
		PG_PORT=5432 \
		PG_DBNAME=chainblocktest \
		PG_USER=postgres \
		PG_PASSWORD=password \
		stack test chainblock

test-db:
	ENV=Test \
		PORT=3000 \
		PG_HOST=localhost \
		PG_PORT=5432 \
		PG_DBNAME=chainblocktest \
		PG_USER=postgres \
		PG_PASSWORD=password \
		stack test chainblock --test-arguments="--match=DB"

test-business:
	ENV=Test \
		stack test chainblock --test-arguments="--match=Business"
