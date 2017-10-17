# Tholos - The Password Manager

This is a password manager using CBC encryption. The project is architected with API/Business/Data layers separated to be composable. The current data store is postgres with the Opaleye ORM . Do to its layer separation, the roadmap for the project is to have a NOSQL implementation as well. This API is a RESTful(ish) JSON API built with the Servant framework.

## Dependencies
* `stack`
* a running postgres instance. Please see the Makefile for the assumed connection string values. 


## Building the Project

```stack build```

## Running Tholos

```make start```

* see the Makefile for DB connection string values if failure occurs.

It is encouraged to set the environment variables as needed in the Makefile. You may find that you need to change them to your particular settings.

## Testing

```make test```
* also requires a running db
