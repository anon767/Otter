if [ -z "$*" ]; then
  echo "Usage: ./run-client.sh <client program> [options...]"
  exit
fi
../../newlib-1.19.0/otter/otter-with-libc src/redis-server_comb.c deps/hiredis/libhiredis.a --domultiotter --doRunRmtmps $*
