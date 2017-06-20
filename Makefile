.PHONY:

start-chainblock:
	ENV=Development \
		PORT=3000 \
		DB_NAME=chainblock \
		DB_USER=postgres \
		DB_PASS=password \
		DB_HOST=localhost \
		stack exec chainblock

test:
	ENV=Test \
		PORT=3000 \
		DB_NAME=chainblock-test \
		DB_USER=postgres \
		DB_PASS=password \
		DB_HOST=localhost \
		stack exec chainblock
