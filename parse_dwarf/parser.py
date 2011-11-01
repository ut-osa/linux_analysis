# linux_analysis project
# File name: parser.py
# 
# Description: 
# 
# Operating Systems & Architecture Group
# University of Texas at Austin - Department of Computer Sciences
# Copyright 2010, 2011. All Rights Reserved.
# See LICENSE file for license terms.

import sys
import re

def parse(f, compile_unit_cb):
	pre_str = r'<(?P<level>\d+)><(?:(?P<offset>\d+)\+)?(?P<id>\d+)>' + \
              '<DW_TAG_(?P<tag>\w+)>(?P<attrs>.*)'
	pre_re = re.compile(pre_str)
	
	attr_str = r'(\w+)<(.*)>'
	attr_re = re.compile(attr_str)

	tag_stack = []
	last_compile_unit = None

	for line in f:
		l = line.strip()
		if len(l) == 0 or l[0] == '.':
			continue

		m = pre_re.match(l)

		tag = m.groupdict()
		tag['attrs'] = dict([attr_re.match(x).groups() for x in tag['attrs'].split(' DW_AT_')[1:]])
		tag['children'] = []

		tag_popped = None
		while len(tag_stack) > 0 and tag_stack[-1]['level'] >= tag['level']:
			tag_popped = tag_stack.pop()

		if len(tag_stack) > 0:
			tag_stack[-1]['children'].append(tag)
		elif tag_popped is not None:
			print >>sys.stderr, tag_popped['attrs']['name']
			compile_unit_cb(tag_popped)
		
		tag_stack.append(tag)
