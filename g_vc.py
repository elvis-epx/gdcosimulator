#!/usr/bin/env python

from g_common import *

read_data("data_vc.txt")
vectors(['Rend', 'Volat', 'RendCor', 'RendAcoes', 'VolatAcoes', 'RendCorAcoes'])

plist = [("Rend", "#c0c", "#fef"), ("RendCor", "#f00", "#fee"), \
         None, \
         ("RendAcoes", "#d70", "#ffe7e0"), ("RendCorAcoes", "#880", "#ffe"), \
         None, \
         ("Volat", "#080", "#efe"), ("VolatAcoes", "#04f", "#def")]

run_graph(plist)
