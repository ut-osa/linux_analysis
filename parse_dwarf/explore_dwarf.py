#!/usr/bin/python
import sys
import cPickle
import heapq

class graph_dist:
	def __init__(self, type, catalog):
		self.type = type
		self.dist = None
		catalog[type] = self

	def __cmp__(self, other):
		if self.dist is None and other.dist is None:
			return 0
		elif self.dist is None:
			return 1
		elif other.dist is None:
			return -1
		
		return self.dist - other.dist

def graph_distances(catalog, type_graph, root):
	dcat = {}

	all_nodes = [graph_dist(type, dcat) for type in catalog.base]
	all_nodes += [graph_dist(type, dcat) for type in catalog.struct]
	all_nodes += [graph_dist(type, dcat) for type in catalog.paths]
	all_nodes += [graph_dist(type, dcat) for type in catalog.anon]

	for x in all_nodes:
		if x.type == root:
			x.dist = 0

	while True:
		heapq.heapify(all_nodes)
		node = heapq.heappop(all_nodes)
		if node.dist is None:
			break
		# print node.type, node.dist
		neighbor_dist = node.dist + 1
		if node.type in type_graph.modified:
			for n in type_graph.modified[node.type]:
				n_dist = dcat[n]
				if n_dist.dist is None:
					n_dist.dist = neighbor_dist
				else:
					n_dist.dist = min(n_dist.dist, neighbor_dist)
					
		if node.type in type_graph.contained:
			for n in type_graph.contained[node.type]:
				n_dist = dcat[n]
				if n_dist.dist is None:
					n_dist.dist = neighbor_dist
				else:
					n_dist.dist = min(n_dist.dist, neighbor_dist)

	return dcat
	

stats = {'structs':0, 'struct_depth':0}

def explore_graph(graph, dcat, root, level = 0, left = 0, explored = None, s_depth = 0, catalog = None):
	if explored is None:
		print 'digraph G {'
		_explored = {}
	else:
		_explored = explored

	_explored[root] = None

	root_q = '"' + root.replace('->', '.') + '"'

	right = left
	off_list = [x/20.0 for x in range(-5, 6)]
	cur_off = 0

	stats['struct_depth'] = max(stats['struct_depth'], s_depth)

	if root in graph.modified:
		for type in graph.modified[root]:
			if type not in _explored and dcat[type].dist > dcat[root].dist:
				right += explore_graph(graph, dcat, type, level+1+off_list[cur_off], right, _explored, s_depth, catalog)
				type = type.replace('->', '.')
				print '"'+type+'"','->',root_q
				cur_off = (cur_off+1)%len(off_list)

	if root in graph.contained:
		for type in graph.contained[root]:
			if type not in _explored and dcat[type].dist > dcat[root].dist:
				if type in catalog.struct:
					stats['structs'] += 1
					new_depth = s_depth + 1
				else:
					new_depth = s_depth
				right += explore_graph(graph, dcat, type, level+1+off_list[cur_off], right, _explored, new_depth, catalog)
				type = type.replace('->', '.')
				print '"'+type+'"','->',root_q
				cur_off = (cur_off+1)%len(off_list)

	right = max(right, left+1)
	_explored[root] = ((right+left)/2.0,level)

	if explored is None:
		max_x = max([x[0] for x in _explored.values()])
		max_y = max([x[1] for x in _explored.values()])

		scale_x = (10*72) / max_x
		scale_y = (7.5*72) / max_y

		for type,pos in _explored.items():
			type = type.replace('->', '.')
			print '"'+type+'"','[pos="%f,%f"]'%(pos[0]*scale_x, 11 - pos[1]*scale_y)
		print '}'
		print >>sys.stderr,stats

	return right - left

#f = file(sys.argv[1])
#graph = cPickle.load(f)
#root = graph.names['structure_type_file_operations']
#explore_graph(graph, root, 0)
