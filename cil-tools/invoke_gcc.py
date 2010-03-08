#!/usr/bin/python
import sys,os,errno

# Wrapper for gcc that first runs each file through the preprocessor
# and saves the results to one side.
#
# make CC="../analysis/cil-tools/invoke_gcc.py -cilo "../analysis/intermediates"
#      CC_OPT=gcc
#
# The -cilo option specifies where to save preprocessed .i files, CC_OPT
# tells the build system to use regular gcc for its automagic compiler option
# detection

i = iter(sys.argv)
exe = i.next()

pre_args = ['gcc']
all_args = ['gcc']
compile_cmd = False
asm_input = False
output = None
output_dir = None
output_base = None

while True:
	try:
		arg = i.next()
	except StopIteration:
		break

	if arg == '-cilo':
		output_base = os.path.abspath(i.next())
		continue

	if arg == '-o':
		all_args.append(arg)
		arg = i.next()
		output_dir,output = os.path.split(arg)
	elif arg == '-c':
		compile_cmd = True
	elif arg[-2:] == '.S':
		asm_input = True
	else:
		pre_args.append(arg)

	all_args.append(arg)


if compile_cmd and not asm_input:
	output_dir = os.path.join(output_base, output_dir)
	output = os.path.join(output_dir, 'i'+output+'.i')
	pre_args += ['-E', '-o', output]
	try:
		os.makedirs(output_dir)
	except OSError as (err, strerror):
		if err != errno.EEXIST:
			raise
	ret = os.spawnvp(os.P_WAIT, 'gcc', pre_args)

ret = os.spawnvp(os.P_WAIT, 'gcc', all_args)
sys.exit(ret)
