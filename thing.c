#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

typedef struct IntArrayPair IntArrayPair;
struct IntArrayPair {
  int* list1;
  int* list2;
  size_t len1;
  size_t len2;
  size_t inx1;
  size_t inx2;
};
typedef struct IntPair IntPair;
struct IntPair {
  int a;
  int b;
};

#ifndef _countof
#define _countof(arr)				\
  sizeof((arr))/sizeof((arr)[0])
#endif

#define MAKE_ARR_PARAM(type,...) _countof(((type[]){__VA_ARGS__})), (type[]){__VA_ARGS__}

IntArrayPair init_list_pair(size_t len1, int list1[len1],
			    size_t len2, int list2[len2]){
  return (IntArrayPair){list1, list2, len1, len2};
}

void list_pair_reset(IntArrayPair p_list_pair VALID_PTR){
  p_list_pair->inx1 = p_list_pair->inx2 = 0;
}

bool gen_next_pair(IntArrayPair gen VALID_PTR,
		   IntPair pair VALID_PTR){
  if((gen->inx1 >= gen->len1) ||
     (gen->inx2 >= gen->len2))
    return false;

  pair->a = gen->list1[gen->inx1];
  pair->b = gen->list2[gen->inx2];
  
  gen->inx1++;
  if(gen->inx1 == gen->len1){
    gen->inx1 = 0;
    gen->inx2++;
  }
  
  return true;  
}

typedef bool filter_pair_fn(void* params, IntPair pair);

bool gen_next_pair_filtered(IntArrayPair gen VALID_PTR,
			    IntPair pair VALID_PTR,
			    size_t fn_count,
			    void* filter_params[fn_count],
			    filter_pair_fn* filter_fnxs[fn_count]){
  while(true){
    if(!gen_next_pair(gen, pair))
      return false;
    for(size_t i = 0; i < fn_count; ++i){
      if(!filter_fnxs[i](filter_params[i], *pair))
	goto loop_back;
    }
    return true;
  loop_back:
    continue;
  }
}

bool filter_not_same(void* param, IntPair pair){
  return pair.a != pair.b;  
}

bool filter_not_in_list(void* other_list, IntPair pair){
  IntArrayPair* other_pairs = other_list;
  IntArrayPair copy = *other_pairs;
  list_pair_reset(&copy);

  IntPair loop_pair;
  while(gen_next_pair(&copy, &loop_pair)){
    if((pair.a == loop_pair.a) && (pair.b == loop_pair.b))
      return false;
  }
  return true;
}
