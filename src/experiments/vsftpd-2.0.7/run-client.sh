if [ -z "$*" ]; then
  echo "Usage: ./run-client.sh <client program> [options...]"
  exit
fi
../../newlib-1.19.0/otter/otter-with-libc vsftpd.cil.c --domultiotter $*
