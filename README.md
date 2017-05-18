# Shadowsocks Erlang
Shadowsocks for erlang port, **sserl** is Erlang/OTP Application build with rebar3

# Usage
```
$ make && rebar3 shell

```

# Configuration

# Runing State

# TODO
- [ ] 抽离出流量统计的 record 表格式
- [x] 增加对 `连接数` 的限制, 连入一个 IP 则认为连入一个客户端。
- [ ] 流量限制, 每月归零
- [ ] 时间限制, 过期后是否任然监听端口 
- [ ] 是否考虑参照 emq, 将非标准的功能, 作为插件, Hook 的形式加入到工程之中