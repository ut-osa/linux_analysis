/*
 * linux_analysis project
 * File name: typedefs.h
 * 
 * Description: 
 * 
 * Operating Systems & Architecture Group
 * University of Texas at Austin - Department of Computer Sciences
 * Copyright 2010, 2011. All Rights Reserved.
 * See LICENSE file for license terms.
 */

#ifndef OSCK_TYPEDEFS_H
#define OSCK_TYPEDEFS_H

struct symbol_type {
   const char *name;
   int type_id;
};
extern struct symbol_type sym_types[];
extern int n_sym_types;

typedef int (*verify_fn_t)(void*);
extern verify_fn_t verify_map[];
extern int max_verify_fn;

extern const char *field_names[];

typedef int typeoff_map_t[2];
extern typeoff_map_t *all_typeoff_maps[];

int ptr_verify(int type_id, int field_name, void *ptr_field_address);

#endif
