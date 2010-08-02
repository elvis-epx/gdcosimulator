import sys
import math
import random

try:
    import psyco
    psyco.full()
except ImportError:
    pass

import blackscholes
try:
    import bsfast
    blackscholes.patch_for_fast()
except ImportError:
    pass

def parse_args():
    log = False
    argv = sys.argv[1:]
    try:
        i = argv.index("--log")
        del argv[i]
        log = True
    except ValueError:
        pass
    args = [ float(s) for s in argv ]
    return args, log
