if [ -z "$*" ]; then
  echo "Usage: ./run-client.sh <client program> [options...]"
  exit
fi
../../otter/otter.pl --merge -nostdinc -isystem../../otter/libc vsftpd.cil.c --domultiotter $*
