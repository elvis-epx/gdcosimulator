#!/usr/bin/env python

from h_common import *

v = []
# nome, std, min, max, step
v.append(["K%", 1.0, 0.9, 1.1, 0.02])
v.append(["volvol%",    0.0, 0.0, 1.0, 0.25])
v.append(["riskpremium%",   1.5, 1.0, 2.0, 0.2])
v.append(["vol%",       0.3, 0.1, 0.5, 0.1])

run_simulation_grid("data_vc.txt", v, "simulavc.py")
