#ifndef TEEVM_H
#define TEEVM_H


typedef struct scheduler_t scheduler_t;
#define SCHEDULER_T_DEFINED
typedef struct code_base_t code_base_t;
#define CODE_BASE_T_DEFINED

#ifndef ATOMS_T_DEFINED
typedef struct atoms_t atoms_t;
#define ATOMS_T_DEFINED
#endif

#ifndef NAMED_TUPLES_T_DEFINED
typedef struct named_tuples_t named_tuples_t;
#define NAMED_TUPLES_T_DEFINED
#endif

typedef struct outlet_mall_t outlet_mall_t;
#define OUTLET_MALL_T_DEFINED

typedef struct teevm_t teevm_t;
struct teevm_t {
	atoms_t *atoms;
	named_tuples_t *nm_tuples;
	code_base_t *base;
	scheduler_t *scheduler;
	outlet_mall_t *mall;
};

#endif
