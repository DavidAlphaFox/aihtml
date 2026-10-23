# 动作里设置的 cookie / 响应头不生效

报的人：beamai_muse　严重度：🟢　回没回：✅ 已实现

## 症状

一个字都不报：在动作里

```erlang
#{req := Req} = aihtml_action:meta(Ctx),
_ = cowboy_req:set_resp_cookie(<<"sid">>, Token, Req, #{http_only => true}),
```

cookie 不会出现在响应里。

## 位置

`apps/aihtml_cowboy/src/aihtml_cowboy_action.erl:48-62` —— `stream/3` 把 `Req0`
放进 `meta`，回复时用的仍是 handler 自己手里的 `Req0`（`json(200, #{ops => Ops}, Req0)`），
动作里对 Req 的修改无处回传。

## 怎么撞上的

beamai_muse 的登录：想把「校验访问密钥 → 发会话 cookie → 跳转」写成一个动作。

## 影响面

需要在动作里设置 cookie / 头的宿主（登录、登出、语言偏好写 cookie）。不报错，容易白写。

下游当前的绕法：登录、登出改走原生表单 POST 到宿主自己的 cowboy handler。

## 建议

- 允许动作通过 `aihtml_action:set_meta/2`（或专门的 `set_cookie/3`）登记要附加的
  `resp_cookies` / `resp_headers`，由传输层在回复前应用；
- 或者在文档里明确写一句「动作无法设置响应头，登录请用原生表单 POST」。
