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

 - compile a program and run with `unix` parameter
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


## systemd integration

 - socket activation
 - journald logging (can be exported for LogView program)
 - auto shutdown on inactivity

With combination of [Systemd Template Unit](https://fedoramagazine.org/systemd-template-unit-files/), `* A` DNS zone and
nginx virtual hosts we can build a "cloud solution" using mORMot services with per-customer process level isolation.
    
### Install Unit    
Add our service to systemd (should be executed once).
This command activate `rest_benchmark` socket on port 8889
```bash
sudo ./installSystemSocket.sh
```

### Socket activation, auto shutdown and logs 
Kill possible instances of RESTBenchmark program
```bash
killall -TERM RESTBenchmark
```

Now program is **NOT running**, lets send an HTTP request to a port configured in `rest_benchmark.socket` unit (8889)
```bash
curl  http://localhost:8889/root/xyz
```
Magic - we got a response :) This is how systemd socket activation works.

Inside our demo program, in case it executed by systemd it:  
 - log all activity into journald (see EchoToConsoleUseJournal)
 - stops after 10 seconds without GET requests (see inactivityWatchdog in RESTBenchmark.dpr)

Open new terminal window and run a command to watch logs from `rest_benchmark` services 
```bash
journalctl -u rest_benchmark.service -f
```

Now wait for 10 second - service should shut down itself.

### journald and LogView (Samples/11 - Exception logging) 

Logs from journald can be exported to format LogView understand. Example:
```bash
journalctl -u rest_benchmark.service --no-hostname -o short-iso-precise --since today | grep "RESTBenchmark\[.*\]:  . " > todaysLog.log
``` 
better to grep by program PID `"RESTBenchmark\[12345]:  . "` to include only one start/stop circle (on 2020-06 LogView do not allow to filter on PIDs)

