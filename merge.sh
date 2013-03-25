#!/bin/bash
for branch in work necromancer; do
    git checkout $branch
    git merge master --no-edit
done
git checkout master
