#!/usr/bin/python

# linux_analysis project
# File name: invoke_gcc.py
# 
# Description: 
# 
# Operating Systems & Architecture Group
# University of Texas at Austin - Department of Computer Sciences
# Copyright 2010, 2011. All Rights Reserved.
# See LICENSE file for license terms.

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

#print >>sys.stderr, "***",sys.argv

pre_args = ['gcc']
all_args = ['gcc']
compile_cmd = False
d_module = False
d_genksyms = False
asm_input = False
output = None
output_dir = None
output_base = None

trouble_files = ['i.tmp_vdso32-setup.o.i', 'i.tmp_syscall_64.o.i']

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
   elif arg == '-DMODULE':
      d_module = True
   elif arg == '-D__GENKSYMS__':
      d_genksyms = True
   elif arg[-2:] == '.S':
      asm_input = True
   else:
      pre_args.append(arg)

   all_args.append(arg)

mc_output = (output is not None) and output.startswith(".tmp_mc_")

if compile_cmd and (not d_module) and (not asm_input) and (not mc_output):
   output_dir = os.path.join(output_base, output_dir)
   output_file = 'i'+output+'.i'
   output = os.path.join(output_dir, output_file)
   pre_args += ['-E', '-o', output]
   try:
      os.makedirs(output_dir)
   except OSError, (err, strerror):
      if err != errno.EEXIST:
         raise

   ret = os.spawnvp(os.P_WAIT, 'gcc', pre_args)
   if ret is not 0:
      sys.exit(ret)

   if output_file not in trouble_files:
      doone_exe = os.path.join(os.path.dirname(sys.argv[0]), 'doone')
      ret = os.spawnvp(os.P_WAIT, doone_exe, [doone_exe, output])
      if ret is not 0:
         sys.exit(ret)
   os.unlink(output)

ret = os.spawnvp(os.P_WAIT, 'gcc', all_args)
sys.exit(ret)
