# Shadowsocks Erlang
Shadowsocks for erlang port, **sserl** is Erlang/OTP Application build with rebar3

***IMPORTANT***: This project forked form: [paulzql/shadowsocks-erlang](https://github.com/paulzql/shadowsocks-erlang)

# Feature
- Mutil-User/Port Manager Interface Support
- Traffic Statistics and Limit
- Max Connection Count limit on port
- Support rc4_md5, aes_128_cfb, aes_192_cfb, aes_256_cfb

# Usage

Install Erlang first
```
brew install erlang
```

Compile & Run:
```
git clone git@github.com:HJianBo/sserl.git
cd sserl
# compile
make

# run
make shell
```
# Deployment

```
make prod-rel
scp _build/prod/rel/sserl/sserl-0.3.3.tar.gz username@server_host:~/

ssh username@server_host
./sserl/bin/sserl start
```

# Configuration
The configuration file path at `./releases/0.3.3/sys.config`

