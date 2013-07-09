#!/bin/sh
erl -Ww -pa ebin deps/*/ebin -s echo 
