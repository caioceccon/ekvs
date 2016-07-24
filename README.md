# EKVS - Erlang Key Value Store
## Deploying on heroku.
Because of a heroku limitation is not possible to try the TCP/UDP features because heroku only forward the port 80.

### Use a different ID ekvs, is already used by this application.
    git clone https://github.com/caioceccon/ekvs
    cd ekvs
    heroku login
    heroku create ekvs --stack cedar --buildpack https://github.com/archaelus/heroku-buildpack-erlang
    git push heroku master
[http://ekvs.herokuapp.com/](http://ekvs.herokuapp.com/)

## Compilation
    git clone https://github.com/caioceccon/ekvs
    cd ekvs
    rebar get-deps compile

## Running
    # Erlang required
    erl -pa ebin deps/*/ebin -s ekvs

## Using TCP version with telnet
    telnet localhost 9023
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    Welcome to Erlang Key Value Store!
    To add a item use `Add:Key:Value`
    To get a item use `Get:Key`
    To get all items use `GetAll:`

## Using UDP version
The udp usage follow the same commands protocol used by the TCP version but it runs in a different port 9024.

## Using the REST version.
The API only accepts json as application type.
The Data resource has the the attributes key and value

    {
      'key': '1',
      'value': 'My Value'
    }

The methods accepted are:
### POST a new data.
    POST localhost:8080/data/

### Get a data.
    GET localhost:8080/data/<ID>

### Get all data.
    GET localhost:8080/data/
