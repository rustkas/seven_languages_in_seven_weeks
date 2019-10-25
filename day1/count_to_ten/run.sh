#!/bin/sh
erl -noshell -pa \
-s count_to_ten test -s init stop
