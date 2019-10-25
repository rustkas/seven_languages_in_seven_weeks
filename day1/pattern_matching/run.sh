#!/bin/sh
erl -noshell -pa \
-s select test -s init stop
