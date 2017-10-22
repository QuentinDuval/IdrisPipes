#!/bin/sh

idris --build test.ipkg
idris --testpkg test.ipkg
