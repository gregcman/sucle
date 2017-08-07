SHELL = /bin/sh
CC    = gcc
LIBS = -lavcodec-ffmpeg -lavformat-ffmpeg -lavutil -lswresample

TARGET  = libprog.so
SOURCES = src/stuff.c
HEADERS = src/stuff.h
OBJECTS = $(SOURCES:.c=.o)

all: $(TARGET)

stuff.o:
	$(CC) -c -Wall -Werror -fpic stuff.c stuff.h

$(TARGET): $(OBJECTS)
	$(CC) -shared -o $(TARGET) $(OBJECTS) $(LIBS)
