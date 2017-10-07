#!/usr/bin/env python
import json
import glob
from collections import OrderedDict
import re


def normalized_type(schema):
    type_ = schema.get("type", [])
    if isinstance(type_, list):
        return type_
    return [type_]


def add_additional_properties(schema):
    if isinstance(schema, dict):
        if "type" in schema and "object" in normalized_type(schema):
            schema["additionalProperties"] = False
        for k, v in schema.items():
            add_additional_properties(v)
    if isinstance(schema, list):
        for v in schema:
            add_additional_properties(v)


def guess_indentation(f):
    f.readline()
    second_line = f.readline()
    f.seek(0)
    match = re.match(" +", second_line)
    if match:
        return len(match.group(0))
    return 2

files = glob.glob("tap_*/schemas/*.json")
for fname in files:
    with open(fname) as f:
        identation = guess_indentation(f)
        schema = json.loads(f.read(), object_pairs_hook=OrderedDict)
    add_additional_properties(schema)
    with open(fname, "w") as f:
        f.write(json.dumps(schema, indent=identation))
        f.write("\n")
