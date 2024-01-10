#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#ifdef UNICODE
#undef UNICODE
#endif
#ifdef _UNICODE
#undef _UNICODE
#endif
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>


typedef struct MappedFile MappedFile;
struct MappedFile {
  void* base_addr;
  size_t file_size;
  struct {
    HANDLE file_handle;
    HANDLE file_map_obj;
  }os_thing;
};

#define VALID_PTR [static 1] 

bool open_read_file(MappedFile file VALID_PTR, const char file_name VALID_PTR);
bool close_read_file(MappedFile file VALID_PTR);

bool open_read_file(MappedFile file VALID_PTR, const char file_name VALID_PTR){

  if(!close_read_file(file))
    return false;
  
  file->os_thing.file_handle = CreateFile(file_name, GENERIC_READ, 0, NULL,OPEN_EXISTING,
					  FILE_ATTRIBUTE_NORMAL, NULL);

  if(file->os_thing.file_handle == NULL){
    goto file_error;
  }

  file->file_size = (size_t)GetFileSize(file->os_thing.file_handle, NULL);

  file->os_thing.file_map_obj = CreateFileMapping(file->os_thing.file_handle,
					  NULL,
					  PAGE_READONLY,
					  0,
					  file->file_size,
					  NULL);

  if(NULL == file->os_thing.file_map_obj){
    goto file_error;
  }

  file->base_addr = MapViewOfFile(file->os_thing.file_map_obj,
				  FILE_MAP_READ, 0, 0,
				  file->file_size);
  if(NULL == file->base_addr){
    goto file_error;
  }

  return true;
 file_error:
  close_read_file(file);
  

  return false;

}

bool close_read_file(MappedFile file VALID_PTR){
  bool error = false;
  if(file->base_addr){
    error = error || (FALSE == UnmapViewOfFile(file->base_addr));
  }
  if(file->os_thing.file_map_obj){
    error = error || (FALSE == CloseHandle(file->os_thing.file_map_obj));
  }
  if(file->os_thing.file_handle){
    error = error || (FALSE == CloseHandle(file->os_thing.file_handle));
  }

  (*file) = (MappedFile){0};
  return !error;
}

size_t get_line_length(size_t len, const char str[len]){

  size_t count = 0;
  while((len != 0) && (*str) && (*(str) != '\n')){
    str++;
    count++;
    len--;
  }
  return count;
}

#ifndef _countof
#define _countof(arr)				\
  sizeof((arr))/sizeof((arr)[0])
#endif

#define MAKE_ARR_PARAM(type,...) _countof(((type[]){__VA_ARGS__})), (type[]){__VA_ARGS__}

bool char_in_str(char ch, size_t len_str, const char* str){
  for(int i = 0; i < len_str; ++i){
    if(ch == str[i])
      return true;
  }
  return false;
}

//Requires that number begin just from start
unsigned long long parse_long(size_t max_len, const char string[max_len]){
  //Detects overflow, returns 
  unsigned long long res = 0;
  for(size_t i = 0; i < max_len; ++i){
    if((string[i] < '0') ||
       (string[i] > '9')){
      return res;
    }
    unsigned long long tmp_res = res * 10 + (int)(string[i] - '0');
    if(tmp_res < res){
      //Overflow occured
      return (unsigned long long)(-1);
    }
    res = tmp_res;
  }
  return res;
}

//Given a start and end of number in line matrix
//Count how many neighbours are there [start, end)
int getthingcount(int row, int start, int end, int width, int height,
		  const char (lines VALID_PTR)[width],
		  size_t compare_len, const char compare_str[compare_len]){

  //Sad:: Couldn't use all that convolution thing i made
  int thingcount = 0;
  for(int j = (start-1) ; j <= end; ++j){
    if(( j < 0) || (j >= width))
      continue;
    for(int i = row-1; i <= (row+1); ++i){
      if((i == row) && ((j != (start-1) ) && (j != end)))
	continue;
      if((i < 0) || ( i >= height))
	continue;
      if (!char_in_str(lines[i][j], compare_len, compare_str)){
	thingcount++;
      }
    }
  }
  return thingcount;
}

unsigned long long dothething(const size_t len,const char string[len]){
  const size_t line_len = get_line_length(len,string);
  const size_t width = (line_len +1);
  const size_t height = len / line_len;

  const char (*lines)[width] = (void*)string;
  unsigned long long sum = 0;
  for(int i = 0; i < height; ++i){


    bool num_area = false;
    int num_inx = 0;
    for(int j = 0; j <= line_len; ++j){
      bool curr_num = char_in_str(lines[i][j],
				  MAKE_ARR_PARAM(char,'0','1','2','3','4','5','6','7','8','9'));
      if(curr_num && !num_area){
	//Num start
	num_inx = j;
      }
      if(!curr_num && num_area){
	//Num end
	int syms = getthingcount(i, num_inx, j, width, height, lines,
				 MAKE_ARR_PARAM(char, ".0123456789\r\n"));
	sum += parse_long(1+j-num_inx,lines[i] + num_inx) * syms;
	
	//Now need to find if at side is some non 
      }
      num_area = curr_num;
    }
  }
  return sum;
}
  
  

int main(int argc, char* argv[argc]){

  const char* infile = "test2.txt";
  if(argc > 1){
    infile = argv[1];
  }

  MappedFile file = {0};
  if(!open_read_file(&file, infile)){
    printf("Error in opening file\n");
    return -1;
  }
  
  printf("Dumping file : %s\n%.*s\n", infile,(int)file.file_size, (char*)file.base_addr);

  printf("The first line is of length %d.\n" , (int)get_line_length(file.file_size, file.base_addr));

  printf("Doing the thing on file %s, sum = %llu\n", infile,
	 dothething(file.file_size, file.base_addr));
	 
  
  if(!close_read_file(&file)){
    printf("Error in closing file\n");
    return -1;
  }

  IntArrayPair pair_set1 = init_list_pair(MAKE_ARR_PARAM(int,1,2,3),
				      MAKE_ARR_PARAM(int,3,4,5,6));

  IntArrayPair pair_set2 = init_list_pair(MAKE_ARR_PARAM(int,1),
				      MAKE_ARR_PARAM(int,4,5));
  IntPair pair;
  
  printf("First set : ");
  while(gen_next_pair(&pair_set1, &pair)){
    printf( " (%d, %d) ", pair.a, pair.b);
  }
  printf("\n");
  
  printf("Second set : ");
  while(gen_next_pair(&pair_set2, &pair)){
    printf( " (%d, %d) ", pair.a, pair.b);
  }
  printf("\n");

  //Need to test null set once:

  printf("Filtered set, where, difference of set1 with non identical pair and set2 : ");
  
  list_pair_reset(&pair_set1);
  list_pair_reset(&pair_set2);
  filter_pair_fn* fnxs[] = {
    filter_not_same,
    filter_not_in_list,
  };
  void* fnx_params[] = {
    NULL,
    (void*)&pair_set2,
  };
  while(gen_next_pair_filtered(&pair_set1,
			       &pair,
			       _countof(fnxs),
			       fnx_params,
			       fnxs)){
    printf(" (%d, %d) ",pair.a, pair.b);
  }
  printf("\n");
  
  return 0;
}
