#!/bin/sh
erl -noshell -pa \
-s item_selector test -s init stop
