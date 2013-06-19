@echo off

set SRC=src\NCC\
set LIB=lib\

del stage0

ghc -O2 src\Stage0\Config.lhs src\Stage0\Monad.lhs src\Stage0\Make.lhs -o stage0.exe
stage0 --src=%SRC% --dist=%DIST%