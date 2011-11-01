#!/usr/bin/python

# linux_analysis project
# File name: get_allocs.py
# 
# Description: 
# 
# Operating Systems & Architecture Group
# University of Texas at Austin - Department of Computer Sciences
# Copyright 2010, 2011. All Rights Reserved.
# See LICENSE file for license terms.

import sys,re
import parser, grapher2

alloc_str = r'.*call\s+\w+ <kmem_cache_alloc>'
alloc_re = re.compile(alloc_str)

inst_str = r'(c0[0-9a-f]+?):'
inst_re = re.compile(inst_str)

mov_str = r'.*\smov\s+(.*),(.*)'
mov_re = re.compile(mov_str)

loc_str = r'^<lowpc=0x(.*?)><highpc=0x(.*?)>([^<]*)(.*)$'
loc_re = re.compile(loc_str)

class subprog:
	all_subprogs = []

	def __init__(self, tag, abstracts):
		if 'name' in tag['attrs']:
			self.name = tag['attrs']['name']
		else:
			origin = tag['attrs']['abstract_origin'][1:-1]
			self.name = abstracts[origin]
		self.low = tag['attrs']['low_pc']
		self.high = tag['attrs']['high_pc']

		subprog.all_subprogs.append(self)

class var_location:
	def __init__(self, tag):
		self.tag = tag
		remain = tag['attrs']['location']
		self.locations = []
		while len(remain):
			m = loc_re.match(remain)
			if m:
				low,high,op,remain = m.groups()
				self.locations.append((low, high))
			else:
				break

class alloc_inst:
	def __init__(self, inst, pc, next_pc):
		self.inst = inst
		self.pc = pc
		self.next_pc = next_pc
		self.handled = False
		
class alloc_parser:
	def __init__(self, alloc_list):
		self.alloc_list = alloc_list

	def _list_vars(self, tag):
		if tag['tag'] == 'variable':
			if 'location' in tag['attrs']:
				v = var_location(tag)
				for l,h in v.locations:
					pc = int(self.low, 16) + int(l, 16)
					pc_str = '%x' % (pc,)
					self.all_lowpcs[pc_str] = v

		for t in tag['children']:
			self._list_vars(t)


	def parse_cb(self, root):
		self.all_lowpcs = {}
		if 'low_pc' in root['attrs']:
			self.low = root['attrs']['low_pc']
		else:
			self.low = '0'
		self._list_vars(root)
		g = grapher2.dwarf_graph(root)
		c = grapher2.type_catalog(g)

		for alloc in self.alloc_list:
			if alloc.next_pc in self.all_lowpcs:
				v = self.all_lowpcs[alloc.next_pc]
				print alloc.inst
				if 'type' in v.tag['attrs']:
					id = v.tag['attrs']['type'][1:-1]
					node = g._nodes[id]
					print c._node_names[node]
				else:
					print v.tag['attrs']['abstract_origin']
				alloc.handled = True


def interpret_inst(line):
	m = mov_re.match(line)
	if m:
		src,dst = m.groups([1,2])

def get_next_inst(f):
	i = 0
	for inst_line in f:
		m = inst_re.match(inst_line)
		if m:
			return (inst_line, m)
		i += 1
		if i > 2:
			raise 'blah!'

def list_allocs(f):
	res = []
	for line in f:
		m = alloc_re.match(line)
		if m:
			i = 0
			inst_line,m = get_next_inst(f)
			pc = m.group(1)
			next_inst,next_m = get_next_inst(f)
			next_pc = next_m.group(1)
			res.append(alloc_inst(inst_line.strip(), pc, next_pc))
	return res


f = file(sys.argv[1])
alloc_list = list_allocs(f)
alloc_parse = alloc_parser(alloc_list)

f = file(sys.argv[2])
parser.parse(f, alloc_parse.parse_cb)

for x in alloc_list:
	if not x.handled:
		print x.pc
