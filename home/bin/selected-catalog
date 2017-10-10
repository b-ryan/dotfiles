#!/usr/bin/env python
import sys
import json
import subprocess
from collections import OrderedDict


tap = sys.argv[1]
chosen_streams = sys.argv[2:]
output = subprocess.check_output([tap, "-c", "config.json", "-d"])
schema = json.loads(output.decode("utf-8"), object_pairs_hook=OrderedDict)
streams = schema["streams"]
for s in streams:
    if not chosen_streams or s["tap_stream_id"] in chosen_streams:
        s["schema"]["selected"] = True
json.dump(schema, fp=sys.stdout, indent=2)
