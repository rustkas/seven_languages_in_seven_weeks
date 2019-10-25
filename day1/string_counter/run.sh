#!/bin/sh
erl -noshell -pa \
-s string_counter test -s init stop
