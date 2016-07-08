#!/bin/bash

old="Copyright (c) <2015-....>, see CONTRIBUTORS"
new="Copyright (c) <2015-${1}>, see CONTRIBUTORS"

sed -i.bu "s/${old}/${new}/g" $(find . -name "*.scala") COPYING

rm $(find . -name "*.bu")

