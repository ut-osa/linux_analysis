# linux_analysis project
# File name: grapher2.py
# 
# Description: 
# 
# Operating Systems & Architecture Group
# University of Texas at Austin - Department of Computer Sciences
# Copyright 2010, 2011. All Rights Reserved.
# See LICENSE file for license terms.

import sys

class dwarf_node:
	def __init__(self, tmp_id):
		# modifiers for this type
		# each type of modifier should have just one
      # node per compile unit
		self.modified = []

		# containers that have this type within
		self.contains = []

		self.id = None
		self._tmp_id = tmp_id
		self.tag = None
		self.attrs = None
		self.explored = 0
		

class dwarf_graph(object):
	def __init__(self, parse):
		assert parse['tag'] == 'compile_unit'

		self.offset = parse['offset'] + '_'
		self._nodes = {}

		self.actions = {
			'union_type':'_union',
			'structure_type':'_structure',

			'volatile_type':'_modifier_void',
			'const_type':'_modifier_void',
			'pointer_type':'_modifier_void',
			'typedef':'_modifier_name',

			'array_type':'_modifier',

			'base_type':'_base_type',
			'subroutine_type':'_base_type',
			'enumeration_type':'_base_type',

			'subprogram':'_code_block',
			'inlined_subroutine':'_code_block',
			'lexical_block':'_code_block',
		}

		self._nodes['void'] = dwarf_node('void')
		self._nodes['void'].id = 'void'

		self._graph_tag_list(parse, None)


	def _graph_tag_list(self, tag, parent):
		for tag in tag['children']:
			fn = self.actions.get(tag['tag'], None)
			if fn is not None:
				fn = self.__getattribute__(fn)
				try: fn(tag, parent)
				except:
					print >>sys.stderr, tag
					raise

	def _code_block(self, tag, parent):
		node = self._create_node(tag, parent)
		self._graph_tag_list(tag, node)

	def _get_node(self, id):
		if self._nodes.has_key(id):
			n = self._nodes[id]
		else:
			n = dwarf_node(id)
			self._nodes[id] = n
		return n

	def _create_node(self, tag, parent):
		node = self._get_node(tag['id'])
		node.tag = tag['tag']
		node.parent = parent
		node.id = tag['id']
		node.attrs = tag['attrs']
		return node

	def _contains(self, src, offset, dst_id):
		dst = self._get_node(dst_id)
		src.contains.append((dst, offset))

	def _modifies(self, src, dst_id):
		dst = self._get_node(dst_id)
		dst.modified.append(src)

	def _structure(self, tag, parent):
		node = self._create_node(tag, parent)

		for member in tag['children']:
			assert member['tag'] == 'member'
			dst_id = member['attrs']['type'][1:-1]
			self._contains(node, member['attrs']['data_member_location'], dst_id)

	def _union(self, tag, parent):
		node = self._create_node(tag, parent)

		for member in tag['children']:
			assert member['tag'] == 'member'
			dst_id = member['attrs']['type'][1:-1]
			self._contains(node, None, dst_id)

	def _base_type(self, tag, parent):
		node = self._create_node(tag, parent)

	def _modifier(self, tag, parent):
		node = self._create_node(tag, parent)
		dst_id = node.attrs['type'][1:-1]
		self._modifies(node, dst_id)

	def _modifier_void(self, tag, parent):
		node = self._create_node(tag, parent)
		if 'type' in node.attrs:
			dst_id = node.attrs['type'][1:-1]
		else:
			dst_id = 'void'
		self._modifies(node, dst_id)

	def _modifier_name(self, tag, parent):
		node = self._create_node(tag, parent)
		# node.tag += '.'+node.attrs['name']
		dst_id = node.attrs['type'][1:-1]
		self._modifies(node, dst_id)

class base_node:
	def __init__(self, size):
		self.size = size

class type_node:
	def __init__(self, name, node):
		self.name = name
		self.file = node.attrs.get('decl_file', '')
		self.contents = []

class path_node:
	def __init__(self, path):
		self.path = path

class type_catalog:
	def __init__(self, dwarf):
		self.base = {'void':None}
		self.struct = {}
		self.paths = set()
		self.anon = {}

		self._node_names = {}

		for mod in dwarf._nodes['void'].modified:
			self._name_modifier(mod, 'void')
		del dwarf._nodes['void']

		for id,node in dwarf._nodes.items():
			if node.tag == 'base_type':
				name = node.attrs['byte_size'] + 'byte'
				bnode = base_node(int(node.attrs['byte_size']))
				self.base[name] = bnode

				for mod in node.modified:
					self._name_modifier(mod, name)

				self._node_names[node] = name

			elif node.tag == 'subroutine_type':
				name = 'f()'
				self.base[name] = None
				for mod in node.modified:
					self._name_modifier(mod, name)
				self._node_names[node] = name

			elif node.parent is None:
				if node.tag == 'structure_type' or node.tag == 'union_type':
					if 'name' in node.attrs:
						if node.tag == 'structure_type':
							name = 'struct.' + node.attrs['name']
						elif node.tag == 'union_type':
							name = 'union.' + node.attrs['name']
					elif len(node.modified):
							name = self._anon_name(node)
					else:
						continue

					self.struct[name] = type_node(name, node)
					for mod in node.modified:
						self._name_modifier(mod, name)
					self._node_names[node] = name

				elif node.tag == 'enumeration_type':
					if 'name' in node.attrs:
						name = node.attrs['name']
					else:
						name = self._anon_name(node)
					self.base[name] = None
					for mod in node.modified:
						self._name_modifier(mod, name)
					self._node_names[node] = name

		self._anon_nodes = set()
		for id,node in dwarf._nodes.items():
			if (node.tag == 'structure_type' or node.tag == 'union_type') and \
					node in self._node_names:
				self._name_children(node, self._node_names[node])
											
	def _anon_name(self, node):
		name = node.tag + '@'
		name += node.attrs['decl_file'].split(' ', 1)[1] + ':'
		name += node.attrs['decl_line']
		return name
		
	def _name_children(self, node, name):
		if name in self.struct:
			parent = self.struct[name]
		else:
			parent = self.anon[name]
		assert len(parent.contents) == 0

		for child,offset in node.contains:
			if child not in self._node_names:
				assert child not in self._anon_nodes
				assert len(child.modified) == 0
				child_name = name + '->'
				if child.tag == 'structure_type':
					child_name += 'struct'
				elif child.tag == 'union_type':
					child_name += 'union'
				else:
					print >>sys.stderr,child.tag,child.id
					assert False
				if offset is not None:
					child_name += '@' + offset.replace('DW_OP_plus_uconst ', '')
				child_type = type_node(child_name, child)
				self.anon[child_name] = child_type
				self._name_children(child, child_name)
				self._anon_nodes.add(child)
			else:
				child_name = self._node_names[child]

			parent.contents.append((child_name, offset))
			

	def _name_modifier(self, node, name):
		tag_map = {
			'volatile_type':'volatile',
			'const_type':'const',
			'pointer_type':'*',
			'typedef':'typedef',
			'array_type':'[]',
		}
		path = tag_map[node.tag] + ' ' + name
		self.paths.add(path)

		for mod in node.modified:
			self._name_modifier(mod, path)

		self._node_names[node] = path

	def _munge(self, type, other_type):
		print >>sys.stderr,type.name,type.file,type.contents
		print >>sys.stderr,other_type.name,other_type.file,other_type.contents
		if len(other_type.contents) > len(type.contents):
			type.contents = other_type.contents
			
	def add(self, other):
		self.base.update(other.base)
		self.paths.update(other.paths)

		for struct,type in self.struct.items():
			if struct in other.struct:
				other_type = other.struct[struct]
				if len(type.contents) == 0:
					type.contents = other_type.contents
				elif len(other_type.contents) > 0:
					if type.contents != other_type.contents:
						self._munge(type, other_type)
				del other.struct[struct]
		self.struct.update(other.struct)

		for struct,type in self.anon.items():
			if struct in other.anon:
				other_type = other.anon[struct]
				if len(type.contents) == 0:
					type.contents = other_type.contents
				elif len(other_type.contents) > 0:
					if type.contents != other_type.contents:
						self._munge(type, other_type)
				del other.anon[struct]
		self.anon.update(other.anon)

class type_graph:
	def __init__(self, catalog):
		self.catalog = catalog
		self.modified = {}
		self.contained = {}

		for type in sorted(catalog.paths, lambda x,y: len(x) - len(y)):
			fier,fied = type.split(' ', 1)
			self.modified.setdefault(fied, set()).add(type)

		for type in catalog.struct:
			for child,offset in catalog.struct[type].contents:
				self.contained.setdefault(child, set()).add(type)

		for type in catalog.anon:
			for child,offset in catalog.anon[type].contents:
				self.contained.setdefault(child, set()).add(type)
