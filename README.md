# Tox clients in Haskell

A Tox client without its UI, on hs-toxcore, hs-toxav and hs-toxsync: chats,
contacts, files, avatars, calls, export and import, and the devices of an
account. hTox is this with a UI.

`Tox.Client.Update` is how events change a client and the commands they give
Tox, as pure functions. `Tox.Client.Network` runs Tox behind it, and plays
calls through hs-toxav's `Tox.AV.Playout`.

## Echo bot

`tools/echobot.hs` sends friends back what they send it: messages, and the
audio and video of their calls. It's rs-toxcore-c's echobot on hs-toxcore,
named echobot-hs so that clients can call both:

```sh
bazel run -c opt //hs-tox/tools:echobot
```

Send it `!help` for the calls it places when asked.
