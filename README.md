# Shadowsocks Erlang
Shadowsocks for erlang port, **sserl** is Erlang/OTP Application build with rebar3

***IMPORTANT***: This project forked form: [paulzql/shadowsocks-erlang][https://github.com/paulzql/shadowsocks-erlang]

# Feature
- Mutil-User/Port Manager Interface Support
- Traffic Statistics and Limit
- Max Connection Count limit on port

# Usage
## By Production .tar
```shell
mkdir sserl
mv somepath/sserl-x.y.z.tar sserl
tar zxf sserl-x.y.z.tar
cd sserl && ./bin/sserl start
```

## By Source
```
make && rebar3 shell
```

# Configuration
Configuration file path: `./releases/x.y.z/sys.config`
