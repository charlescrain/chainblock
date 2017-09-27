# Tholos - The Password Manager

This is a password manager using CBC encryption. The project is architected with API/Business/Data layers separated to be composable. The current data store is powered by the Opaleye ORM on top of postgres. Do to its layer separation, the roadmap for the project is to have NOSQL implementations as well. This API is a RESTful API built with the Servant framework.

## Dependencies
* `stack`
* a running postgres instance


## Building the Project

```stack build```

## Running Tholos

```make start-Tholos```

It is encouraged to set the environment variables as needed in the Makefile. You may find that you need to change them to your particular settings.

## Testing

```make test-db```
