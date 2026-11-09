#!/usr/bin/env bash
# Runs hTox's client (group-peer) against a c-toxcore client (tox-share-peer),
# which also runs a local network of DHT nodes with a TCP relay. hTox adds the
# C client, founds a private group and invites it. Each says something, the C
# client shares a file that hTox downloads, and asks hTox for the group's
# history, which must have the C client's message.
set -eu

C_PEER="$1"
HTOX="$2"
OWN_WORK=
if [ -n "${TEST_TMPDIR:-}" ]; then
  WORK="$TEST_TMPDIR"
else
  WORK=$(mktemp -d)
  OWN_WORK=1
fi

C_PID=
HTOX_PID=
cleanup() {
  kill $C_PID $HTOX_PID 2>/dev/null || true
  wait 2>/dev/null || true
  if [ -n "$OWN_WORK" ]; then
    rm -r "$WORK"
  fi
}
trap cleanup EXIT

# wait_for FILE PATTERN SECONDS
wait_for() {
  for _ in $(seq $(($3 * 10))); do
    if grep -q "$2" "$1"; then
      return 0
    fi
    sleep 0.1
  done
  return 1
}

fail() {
  echo "FAIL: $1"
  for log in "$WORK"/*.log; do
    echo "=== $log"
    grep -v '^LOG ' "$log" | tail -n 50
  done
  exit 1
}

head -c 300000 /dev/urandom >"$WORK/payload.bin"
mkdir -p "$WORK/received"

"$C_PEER" --dht-nodes 8 --tcp-relay-port $((20000 + RANDOM % 40000)) --share "$WORK/payload.bin" --say "hello from c" \
  >"$WORK/c-peer.log" 2>&1 &
C_PID=$!
wait_for "$WORK/c-peer.log" '^TOX_ID ' 10 || fail "C client did not start"

# hTox bootstraps only from TCP relays, so the relay's node, with its UDP port.
RELAY=$(sed -n 's/^TCP_RELAY //p' "$WORK/c-peer.log")
NODE_PORT=$(sed -n 's/^DHT_NODE [^:]*:\([0-9]*\):.*/\1/p' "$WORK/c-peer.log" | head -n 1)
NODE="${RELAY%%:*}:$NODE_PORT:${RELAY#*:}"

"$HTOX" --node "$NODE" --add "$(sed -n 's/^TOX_ID //p' "$WORK/c-peer.log")" --received "$WORK/received" --say "hello from htox" \
  >"$WORK/htox.log" 2>&1 &
HTOX_PID=$!

wait_for "$WORK/htox.log" '^FRIEND_ONLINE$' 30 || fail "hTox did not see the C client online"
wait_for "$WORK/htox.log" '^MEMBER_JOINED ' 20 || fail "the C client did not join hTox's group"
wait_for "$WORK/c-peer.log" '^MESSAGE hello from htox$' 10 || fail "the C client did not get hTox's message"
wait_for "$WORK/htox.log" '^MESSAGE .* hello from c$' 10 || fail "hTox did not get the C client's message"
wait_for "$WORK/htox.log" '^DOWNLOADED ' 30 || fail "hTox did not download the shared file"
[ "$(sed -n 's/^DOWNLOADED [^ ]* 300000 //p' "$WORK/htox.log")" = "$(sha1sum "$WORK/payload.bin" | cut -d' ' -f1)" ] ||
  fail "hTox downloaded a different file"
wait_for "$WORK/c-peer.log" '^HISTORY_TEXT hello from c$' 20 || fail "hTox did not answer the history request"

echo "PASS"
