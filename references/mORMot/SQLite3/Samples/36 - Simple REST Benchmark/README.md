# Simple mORMot server for REST benchmark

## Socket based server

 - compile and run RESTBenchmark
 - test it with browser:
    - http://localhost:8888/root/abc
    - http://localhost:8888/root/xyz
 - test it with Apache Bench
```
ab -n 10000 -c 1000 http://localhost:8888/root/abc 
```

## Keep alive
 By default mROMot HTTP server runs in KeepAlive mode.

 To disable KeepAlive run `RESTBenchmark` with secont parameter `false`
```
 ./RESTBenchmark 8888 false
```

Disabling KeepAlive make sence in case mORMotserver is behind the reverse proxy.
In this case reverse proxy cares about KeepAlive connection pool and mormot can
operate with fixed thread pool size.

## Unix Domain Socket (Linux)

### When to use
In case mORMot is behind a local reverse proxy on the environment with a
huge number of incoming connection it's make sence to use a UDS to minimize
unnecessary TCP handshakes between mORMot and reverse proxy.

To emulate such environment on the syntetic test we can disable keep alive
in RESTBEnchmark by passing `false` to then second parameter
```
./RESTBenchmark unix false
./RESTBenchmark 8888 false
```

### How to run

 - compile program and run with `unix` parameter
```
./RESTBenchmark unix
```

 Benchmark will listen on Unix Domain Socket `/tmp/rest-bench.socket`

 - test it with curl
```
curl --unix-socket /tmp/rest-bench.socket http://localhost/root/abc
```

 - setup nginx as a reverse proxy
```
sudo ln -s "$(pwd)/mormot-rest-nginx.conf" /etc/nginx/sites-available
sudo ln -s /etc/nginx/sites-available/mormot-rest-nginx.conf /etc/nginx/sites-enabled
sudo nginx -s reload
```

 - test it using ab (or better - wrk)
```
wrk http://localhost:8888/root/abc
```
