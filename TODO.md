# TODOs

## Features
- 添加 Dashboard 页面以便于管理?
- 使用 `cuttlefish` 管理配置文件
- 多台机器集群处理
- 支持 Linux 系机器开机启动
- CLIs 支持
- 将流量统计和连接鉴权抽成插件的思想去做
    - 认证插件; 判断这个连接是否被允许
    - 统计插件; 可以存储具体流量的使用情况
- 支持一个端口多个密码？？密码=用户

## Improve
- Socket每次流量上报时, 需要判断是否超限制然后断开连接
- Mutil-Manager 的数据接口是否需要加密
- sserl client 支持
- 修改为依赖 esockd


## Plan

esockd --> launch logic --> 0.4.0

auth, traffic report --> hooks --> 0.5.0

culltefish --> 0.6.0
