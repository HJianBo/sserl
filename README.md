sserl
=====

An OTP application

Build
-----

    $ rebar3 compile


# TODO
- 抽离出流量统计的 record 表格式

- 增加限制
 1. 增加对 `连接数` 的限制:
   - 连入一个 IP 则认为连入一个客户端。

- 每月的流量限制

- 时间限制, 过期后是否任然监听端口 