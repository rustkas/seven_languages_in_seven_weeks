#!/bin/sh
erl -noshell -pa \
-s tic_tac_toe test -s init stop
