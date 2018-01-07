SHELL = /bin/sh
CC    = gcc
LIBS = -lavcodec -lavformat -lavutil -lswresample #-lavcodec-ffmpeg -lavformat-ffmpeg -lavutil -lswresample

TARGET  = libprog.so
OBJECTS = $(SOURCES:.c=.o)

all: libprog.so

stuff.o:
	$(CC) -c -Wall -Werror -fpic src/stuff.c include/stuff.h

libprog.so: stuff.o
	$(CC) -shared -o libprog.so stuff.o $(LIBS)

clean:
	rm stuff.o
	rm libprog.so
