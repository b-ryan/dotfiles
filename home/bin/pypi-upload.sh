#!/bin/bash
rm -r build dist
find . -name \*.pyc -exec rm {} \;
find . -name __pycache__ -exec rm -r {} \;
./setup.py bdist_wheel
twine upload dist/*
