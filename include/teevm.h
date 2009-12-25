#ifndef TEEVM_H
#define TEEVM_H

typedef struct scheduler_t scheduler_t;
typedef struct code_base_t code_base_t;
typedef struct atoms_t atoms_t;
typedef struct named_tuples_t named_tuples_t;
typedef struct outlet_mall_t outlet_mall_t;

typedef struct teevm_t teevm_t;
struct teevm_t {
	atoms_t *atoms;
	named_tuples_t *nm_tuples;
	code_base_t *base;
	scheduler_t *scheduler;
	outlet_mall_t *mall;
};

#endif