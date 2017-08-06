# -----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# Lukas Niederbremer <webmaster@flippeh.de> and Clark Gaebel <cg.wowus.cg@gmail.com>
# wrote this file. As long as you retain this notice you can do whatever you
# want with this stuff. If we meet some day, and you think this stuff is worth
# it, you can buy us a beer in return.
# -----------------------------------------------------------------------------

CFLAGS=-g -Wall -Wextra -std=c99 -pedantic -fPIC

all: nbtreader check

nbtreader: main.o libnbt.a
	$(CC) $(CFLAGS) main.o -L. -lnbt -lz -o nbtreader

check: check.c libnbt.a
	$(CC) $(CFLAGS) check.c -L. -lnbt -lz -o check

regioninfo: regioninfo.c libnbt.a
	$(CC) $(CFLAGS) regioninfo.c -L. -lnbt -lz -o regioninfo

test: check
	cd testdata && ls -1 *.nbt | xargs -n1 valgrind ../check && cd ..

main.o: main.c

libnbt.a: buffer.o nbt_loading.o nbt_parsing.o nbt_treeops.o nbt_util.o
	ar -rcs libnbt.a buffer.o nbt_loading.o nbt_parsing.o nbt_treeops.o nbt_util.o

buffer.o: buffer.c
nbt_loading.o: nbt_loading.c
nbt_parsing.o: nbt_parsing.c
nbt_treeops.o: nbt_treeops.c
nbt_util.o: nbt_util.c
