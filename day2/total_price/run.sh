#!/bin/sh
erl +pc unicode -noshell -pa \
-s total_price test -s init stop
