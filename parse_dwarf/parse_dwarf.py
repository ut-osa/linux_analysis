#!/usr/bin/python

# linux_analysis project
# File name: parse_dwarf.py
# 
# Description: 
# 
# Operating Systems & Architecture Group
# University of Texas at Austin - Department of Computer Sciences
# Copyright 2010, 2011. All Rights Reserved.
# See LICENSE file for license terms.


import sys
import parser,grapher2
import explore_dwarf
import cPickle

combined = None

def parse_cb(root):
	global combined
	g = grapher2.dwarf_graph(root)
	c = grapher2.type_catalog(g)

	if combined:
		combined.add(c)
	else:
		combined = c


f = file(sys.argv[1])
parser.parse(f, parse_cb)

g = grapher2.type_graph(combined)
d = explore_dwarf.graph_distances(combined, g, 'struct.file_operations')
explore_dwarf.explore_graph(g, d, 'struct.file_operations', catalog=combined)
