 
#include <stdlib.h> 
#include <stdio.h>
#include <unistd.h>
#include <string.h> 
 
#define uint32 unsigned int
int insertion_sort_bound= 8; /* boundary point to use insertion sort */
 
typedef int (*CMPFUN)(int, int);
typedef enum {
    BUBBLE,
    SELECTION,
    INSERTION,
    QUICK,
    MERGE,
    HEAP,
    SHELL,
    COMBO
} SortType;
 
void usage(int code, char *programName) {
  printf("Usage: %s [options: see source code] data_set_file [params_file]\n", programName);
  exit(code);
}

void BubbleSort(int This[], CMPFUN fun_ptr, uint32 ub)
{
  /* bubble sort */
  uint32 indx;
  uint32 indx2;
  int temp;
  int temp2;
  int flipped;

  if (ub <= 1)
    return;
  indx = 0; 
  do
  {
    flipped = 0;
    for (indx2 = ub - 1; indx2 > indx; indx2--)
    {
      temp = This[indx2];
      temp2 = This[indx2 - 1];
      if ((*fun_ptr)(temp2, temp) > 0)
      {
          This[indx2 - 1] = temp;
          This[indx2] = temp2;
          flipped = 1;
      }
    }
  } while ((++indx < ub) && flipped);

}

void SelectionSort(int This[], CMPFUN fun_ptr, uint32 the_len)
{
  /* selection sort */

  uint32 indx;
  uint32 indx2;
  uint32 large_pos;
  int temp;
  int large;

  if (the_len <= 1)
    return;

  for (indx = the_len - 1; indx > 0; --indx)
  {
    /* find the largest number, then put it at the end of the array */
    large = This[0];
    large_pos = 0;

    for (indx2 = 1; indx2 <= indx; ++indx2)
    {
      temp = This[indx2];
      if ((*fun_ptr)(temp ,large) > 0)
      {
        large = temp;
        large_pos = indx2;
      }
    }
    This[large_pos] = This[indx];
    This[indx] = large;
  }
}

void InsertionSort(int This[], CMPFUN fun_ptr, uint32 the_len)
{
  /* insertion sort */

  uint32 indx;
  int cur_val;
  int prev_val;

  if (the_len <= 1)
    return;

  prev_val = This[0];

  for (indx = 1; indx < the_len; ++indx)
  {
    cur_val = This[indx];
    if ((*fun_ptr)(prev_val, cur_val) > 0)
    {
      /* out of order: array[indx-1] > array[indx] */
      uint32 indx2;
      This[indx] = prev_val; /* move up the larger item first */

      /* find the insertion point for the smaller item */
      for (indx2 = indx - 1; indx2 > 0;)
      {
        int temp_val = This[indx2 - 1];
        if ((*fun_ptr)(temp_val, cur_val) > 0)
        {
          This[indx2--] = temp_val;
          /* still out of order, move up 1 slot to make room */
        }
        else
          break;
      }
      This[indx2] = cur_val; /* insert the smaller item right here */
    }
    else
    {
      /* in order, advance to next element */
      prev_val = cur_val;
    }
  }
}
  
void QuickSort(int This[], CMPFUN fun_ptr, uint32 first, uint32 last)
{
  uint32 stack_pointer = 0;
  int first_stack[32];
  int last_stack[32];

  for (;;)
  {
    if (last - first <= insertion_sort_bound)
    {
      /* for small sort, use insertion sort */
      uint32 indx;
      int prev_val = This[first];
      int cur_val;

      for (indx = first + 1; indx <= last; ++indx)
      {
        cur_val = This[indx];
        if ((*fun_ptr)(prev_val, cur_val) > 0)
        {
          /* out of order: array[indx-1] > array[indx] */
          uint32 indx2;
          This[indx] = prev_val; /* move up the larger item first */

          /* find the insertion point for the smaller item */
          for (indx2 = indx - 1; indx2 > first; )
          {
            int temp_val = This[indx2 - 1];
            if ((*fun_ptr)(temp_val, cur_val) > 0)
            {
              This[indx2--] = temp_val;
              /* still out of order, move up 1 slot to make room */
            }
            else
              break;
          }
          This[indx2] = cur_val; /* insert the smaller item right here */
        }
        else
        {
          /* in order, advance to next element */
          prev_val = cur_val;
        }
      }
    }
    else
    {
      int pivot;
 
      /* try quick sort */
      {
        int temp;
        uint32 med = (first + last) >> 1;
        /* Choose pivot from first, last, and median position. */
        /* Sort the three elements. */
        temp = This[first];
        if ((*fun_ptr)(temp, This[last]) > 0)
        {
          This[first] = This[last]; This[last] = temp;
        }
        temp = This[med];
        if ((*fun_ptr)(This[first], temp) > 0)
        {
          This[med] = This[first]; This[first] = temp;
        }
        temp = This[last];
        if ((*fun_ptr)(This[med], temp) > 0)
        {
          This[last] = This[med]; This[med] = temp;
        }
        pivot = This[med];
      }
      {
        uint32 up;
        {
	  uint32 down;
          /* First and last element will be loop stopper. */
	  /* Split array into two partitions. */
	  down = first;
	  up = last;
	  for (;;)
	  {
	    do
	    {
	      ++down;
	    } while ((*fun_ptr)(pivot, This[down]) > 0);
 
	    do
	    {
	      --up;
	    } while ((*fun_ptr)(This[up], pivot) > 0);
 
	    if (up > down)
	    {
	      int temp;
	      /* interchange L[down] and L[up] */
	      temp = This[down]; This[down]= This[up]; This[up] = temp;
	    }
	    else
	      break;
	  }
	}
	{
	  uint32 len1; /* length of first segment */
	  uint32 len2; /* length of second segment */
	  len1 = up - first + 1;
	  len2 = last - up;
	  /* stack the partition that is larger */
	  if (len1 >= len2)
	  {
	    first_stack[stack_pointer] = first;
	    last_stack[stack_pointer++] = up;
 
	    first = up + 1;
	    /*  tail recursion elimination of
	     *  Qsort(This,fun_ptr,up + 1,last)
	     */
	  }
	  else
	  {
	    first_stack[stack_pointer] = up + 1;
	    last_stack[stack_pointer++] = last;

	    last = up;
	    /* tail recursion elimination of
	     * Qsort(This,fun_ptr,first,up)
	     */
	  }
	}
        continue;
      }
      /* end of quick sort */
    }
    if (stack_pointer > 0)
    {
      /* Sort segment from stack. */
      first = first_stack[--stack_pointer];
      last = last_stack[stack_pointer];
    }
    else
      break;
  } /* end for */
}
 
 
void MergeSort(int This[], CMPFUN fun_ptr, uint32 the_len)
{
  uint32 span;
  uint32 lb;
  uint32 ub;
  uint32 indx;
  uint32 indx2;
 
  if (the_len <= 1)
    return;
 
  span = insertion_sort_bound;
 
  /* insertion sort the first pass */
  { 
    int prev_val;
    int cur_val;
    int temp_val;

    for (lb = 0; lb < the_len; lb += span)
    {
      if ((ub = lb + span) > the_len) ub = the_len;

      prev_val = This[lb];
 
      for (indx = lb + 1; indx < ub; ++indx)
      {
        cur_val = This[indx];

        if ((*fun_ptr)(prev_val, cur_val) > 0)
        {
          /* out of order: array[indx-1] > array[indx] */
          This[indx] = prev_val; /* move up the larger item first */

          /* find the insertion point for the smaller item */
          for (indx2 = indx - 1; indx2 > lb;)
          {
            temp_val = This[indx2 - 1];
            if ((*fun_ptr)(temp_val, cur_val) > 0)
            {
              This[indx2--] = temp_val;
              /* still out of order, move up 1 slot to make room */
            }
            else
              break;
          }
          This[indx2] = cur_val; /* insert the smaller item right here */
        }
        else
        {
          /* in order, advance to next element */
          prev_val = cur_val;
        }
      }
    }
  }

  /* second pass merge sort */
  {
    uint32 median;
    int* aux;

    aux = (int*) malloc(sizeof(int) * the_len / 2);
 
    while (span < the_len)
    {
      /* median is the start of second file */
      for (median = span; median < the_len;)
      {
        indx2 = median - 1;
        if ((*fun_ptr)(This[indx2], This[median]) > 0)
        {
          /* the two files are not yet sorted */
          if ((ub = median + span) > the_len)
          {
            ub = the_len;
          }

          /* skip over the already sorted largest elements */
          while ((*fun_ptr)(This[--ub], This[indx2]) >= 0)
          {
          }

          /* copy second file into buffer */
          for (indx = 0; indx2 < ub; ++indx)
          {
            *(aux + indx) = This[++indx2];
          }
          --indx;
          indx2 = median - 1;
          lb = median - span;
          /* merge two files into one */
          for (;;)
          {
            if ((*fun_ptr)(*(aux + indx), This[indx2]) >= 0)
            {
              This[ub--] = *(aux + indx);
              if (indx > 0) --indx;
              else
              {
                /* second file exhausted */
                for (;;)
                {
                  This[ub--] = This[indx2];
                  if (indx2 > lb) --indx2;
                  else goto mydone; /* done */
                }
              }
            }
            else
            {
              This[ub--] = This[indx2];
              if (indx2 > lb) --indx2;
              else
              {
                /* first file exhausted */
                for (;;)
                {
                  This[ub--] = *(aux + indx);
                  if (indx > 0) --indx;
                  else goto mydone; /* done */
                }
              }
            }
          }
        }
        mydone:
        median += span + span;
      }
      span += span;
    }
 
    free(aux);
  } 
}

 
void HeapSort(int This[], CMPFUN fun_ptr, uint32 the_len)
{
  /* heap sort */

  uint32 half;
  uint32 parent;

  if (the_len <= 1)
    return;

  half = the_len >> 1;
  for (parent = half; parent >= 1; --parent)
  {
    int temp;
    int level = 0;
    uint32 child;

    child = parent;
    /* bottom-up downheap */

    /* leaf-search for largest child path */
    while (child <= half)
    {
      ++level;
      child += child;
      if ((child < the_len) &&
          ((*fun_ptr)(This[child], This[child - 1]) > 0))
        ++child;
    }
    /* bottom-up-search for rotation point */
    temp = This[parent - 1];
    for (;;)
    {
      if (parent == child)
        break;
      if ((*fun_ptr)(temp, This[child - 1]) <= 0)
        break;
      child >>= 1;
      --level;
    }
    /* rotate nodes from parent to rotation point */
    for (;level > 0; --level)
    {
      This[(child >> level) - 1] =
        This[(child >> (level - 1)) - 1];
    }
    This[child - 1] = temp;
  }

  --the_len;
  do
  {
    int temp;
    int level = 0;
    uint32 child;

    /* move max element to back of array */
    temp = This[the_len];
    This[the_len] = This[0];
    This[0] = temp;

    child = parent = 1;
    half = the_len >> 1;

    /* bottom-up downheap */

    /* leaf-search for largest child path */
    while (child <= half)
    {
      ++level;
      child += child;
      if ((child < the_len) &&
          ((*fun_ptr)(This[child], This[child - 1]) > 0))
        ++child;
    }
    /* bottom-up-search for rotation point */
    for (;;)
    {
      if (parent == child)
        break;
      if ((*fun_ptr)(temp, This[child - 1]) <= 0)
        break;
      child >>= 1;
      --level;
    }
    /* rotate nodes from parent to rotation point */
    for (;level > 0; --level)
    {
      This[(child >> level) - 1] =
        This[(child >> (level - 1)) - 1];
    }
    This[child - 1] = temp;
  } while (--the_len >= 1);
}  


/* Calculated from the combinations of  9 * (4^n - 2^n) + 1,
 * and  4^n - 3 * 2^n + 1
 */
uint32 hop_array[] =
{
1,
5,
19,
41,
109,
209,
505,
929,
2161,
3905,
8929,
16001,
36289,
64769,
146305,
260609,
587521,
1045505,
2354689,
4188161,
9427969,
16764929,
37730305,
67084289,
150958081,
268386305,
603906049,
1073643521,
2415771649,
0xffffffff }; 
 
 
void ShellSort(int This[], CMPFUN fun_ptr, uint32 the_len)
{
  /* shell sort */

  int level;

  for (level = 0; the_len > hop_array[level]; ++level);

  do
  {
    uint32 dist;
    uint32 indx;
    
    dist = hop_array[--level];
    for (indx = dist; indx < the_len; ++indx)
    {
      int cur_val;
      uint32 indx2;

      cur_val = This[indx];
      indx2 = indx;
      do
      {
        int early_val;
        early_val = This[indx2 - dist];
        if ((*fun_ptr)(early_val, cur_val) <= 0)
          break;
        This[indx2] = early_val;
        indx2 -= dist;
      } while (indx2 >= dist);
      This[indx2] = cur_val;
    }
  } while (level >= 1);
}

 
void HelperHeapSort(int This[], CMPFUN fun_ptr, uint32 first, uint32 the_len)
{
  /* heap sort */

  uint32 half;
  uint32 parent;

  if (the_len <= 1)
    return;

  half = the_len >> 1;
  for (parent = half; parent >= 1; --parent)
  {
    int temp;
    int level = 0;
    uint32 child;

    child = parent;
    /* bottom-up downheap */

    /* leaf-search for largest child path */
    while (child <= half)
    {
      ++level;
      child += child;
      if ((child < the_len) &&
          ((*fun_ptr)(This[first + child], This[first + child - 1]) > 0))
        ++child;
    }
    /* bottom-up-search for rotation point */
    temp = This[first + parent - 1];
    for (;;)
    {
      if (parent == child)
        break;
      if ((*fun_ptr)(temp, This[first + child - 1]) <= 0)
        break;
      child >>= 1;
      --level;
    }
    /* rotate nodes from parent to rotation point */
    for (;level > 0; --level)
    {
      This[first + (child >> level) - 1] =
        This[first + (child >> (level - 1)) - 1];
    }
    This[first + child - 1] = temp;
  }

  --the_len;
  do
  {
    int temp;
    int level = 0;
    uint32 child;

    /* move max element to back of array */
    temp = This[first + the_len];
    This[first + the_len] = This[first];
    This[first] = temp;

    child = parent = 1;
    half = the_len >> 1;

    /* bottom-up downheap */

    /* leaf-search for largest child path */
    while (child <= half)
    {
      ++level;
      child += child;
      if ((child < the_len) &&
          ((*fun_ptr)(This[first + child], This[first + child - 1]) > 0))
        ++child;
    }
    /* bottom-up-search for rotation point */
    for (;;)
    {
      if (parent == child)
        break;
      if ((*fun_ptr)(temp, This[first + child - 1]) <= 0)
        break;
      child >>= 1;
      --level;
    }
    /* rotate nodes from parent to rotation point */
    for (;level > 0; --level)
    {
      This[first + (child >> level) - 1] =
        This[first + (child >> (level - 1)) - 1];
    }
    This[first + child - 1] = temp;
  } while (--the_len >= 1);
}

 
/* explain function
 * Description:
 *   fixarray::Qsort() is an internal subroutine that implements quick sort.
 *
 * Return Value: none
 */
void ComboSort(int This[], CMPFUN fun_ptr, uint32 first, uint32 last)
{
  uint32 stack_pointer = 0;
  int first_stack[32];
  int last_stack[32];

  for (;;)
  {
    if (last - first <= insertion_sort_bound)
    {
      /* for small sort, use insertion sort */
      uint32 indx;
      int prev_val = This[first];
      int cur_val;

      for (indx = first + 1; indx <= last; ++indx)
      {
        cur_val = This[indx];
        if ((*fun_ptr)(prev_val, cur_val) > 0)
        {
          uint32 indx2;
          /* out of order */
          This[indx] = prev_val;

          for (indx2 = indx - 1; indx2 > first; --indx2)
          {
            int temp_val = This[indx2 - 1];
            if ((*fun_ptr)(temp_val, cur_val) > 0)
            {
              This[indx2] = temp_val;
            }
            else
              break;
          }
          This[indx2] = cur_val;
        }
        else
        {
          /* in order, advance to next element */
          prev_val = cur_val;
        }
      }
    }
    else
    {
      int pivot;
 
      /* try quick sort */
      {
	int temp;
	uint32 med = (first + last) >> 1;
        /* Choose pivot from first, last, and median position. */
        /* Sort the three elements. */
        temp = This[first];
        if ((*fun_ptr)(temp, This[last]) > 0)
        {
          This[first] = This[last]; This[last] = temp;
        }
        temp = This[med];
        if ((*fun_ptr)(This[first], temp) > 0)
        {
          This[med] = This[first]; This[first] = temp;
        }
        temp = This[last];
        if ((*fun_ptr)(This[med], temp) > 0)
        {
          This[last] = This[med]; This[med] = temp;
        }
        pivot = This[med];
      }
      {
        uint32 up;
        {
          uint32 down;
          /* First and last element will be loop stopper. */
          /* Split array into two partitions. */
          down = first;
          up = last;
          for (;;)
	  {
	    do
	    {
	      ++down;
	    } while ((*fun_ptr)(pivot, This[down]) > 0);
 
	    do
	    {
              --up;
            } while ((*fun_ptr)(This[up], pivot) > 0);
 
	    if (up > down)
	    {
	      int temp;
              /* interchange L[down] and L[up] */
              temp = This[down]; This[down]= This[up]; This[up] = temp;
	    }
	    else
	      break;
	  }
	}
        {
          uint32 len1; /* length of first segment */
          uint32 len2; /* length of second segment */
          len1 = up - first + 1;
          len2 = last - up;
          if (len1 >= len2)
          {
            if ((len1 >> 5) > len2)
            {
              /* badly balanced partitions, heap sort first segment */
              HelperHeapSort(This, fun_ptr, first, len1);
            }
            else
            {
              first_stack[stack_pointer] = first; /* stack first segment */
              last_stack[stack_pointer++] = up;
            } 
            first = up + 1;
            /*  tail recursion elimination of
             *  Qsort(This,fun_ptr,up + 1,last)
             */
          }
          else
          {
            if ((len2 >> 5) > len1)
            {
              /* badly balanced partitions, heap sort second segment */
              HelperHeapSort(This, fun_ptr, up + 1, len2);
            }
            else
            {
              first_stack[stack_pointer] = up + 1; /* stack second segment */
              last_stack[stack_pointer++] = last;
            }
            last = up;
            /* tail recursion elimination of
             * Qsort(This,fun_ptr,first,up)
             */
          }
        }
        continue;
      }
      /* end of quick sort */
    }
    if (stack_pointer > 0)
    {
      /* Sort segment from stack. */
      first = first_stack[--stack_pointer];
      last = last_stack[stack_pointer];
    }
    else
      break;
  } /* end for */
}
 
 
//  Qsort(This, fun_ptr, 0, the_len - 1);
 
#define ARRAY_SIZE 10

int * my_array = NULL;
    
void fill_array(int num_eles)
{
  int indx;
  my_array = malloc(num_eles * sizeof(int));

  for (indx=0; indx < num_eles; ++indx)
  {
    my_array[indx] = rand();
  }
}

int read_from_file(char * filename, int num_eles) {
    FILE * file = fopen(filename, "rb");
    char line [5];
    int count = 0, len = 0, i = 0;
    int * temp_array = malloc(num_eles * sizeof(int));

    while (fgets(line, sizeof(line), file))
    {
        len = strlen(line);
        if(line[len-1] == '\n') line[len-1] = '\0';
        if(count >= num_eles) {
            my_array = temp_array;
            temp_array = malloc(2 * count * sizeof(int));
            memcpy(temp_array,my_array,count * sizeof(int));
            my_array = NULL;
            num_eles = count * 2;
        }
        if(len > 1) {
          temp_array[count] = atoi(line);
          count++;
        }
    }
    my_array = malloc(count * sizeof(int));
    memcpy(my_array,temp_array,count * sizeof(int));
    fclose(file);
    return count;
} 

int cmpfun(int a, int b)
{
  if (a > b)
    return 1;
  else if (a < b)
    return -1;
  else
    return 0;
}
 
int main(int argc, char* argv[])
{
    int indx;
    char * filename = NULL;
    int sortType = BUBBLE;
    int num_eles = ARRAY_SIZE;
    int opt = 0;
    for (opt = 0; (opt = getopt(argc, argv, "f:s:n:")) != -1; ) {
        switch (opt) {
          case 'f': filename = optarg; break;
          case 's': 
            if(!strcmp(optarg,"bubble")) {
                sortType=BUBBLE;
            } else if(!strcmp(optarg,"selection")) {
                sortType=SELECTION;
            } else if(!strcmp(optarg,"insertion")) {
                sortType=INSERTION;
            } else if(!strcmp(optarg,"quick")) {
                sortType=QUICK;
            } else if(!strcmp(optarg,"merge")) {
                sortType=MERGE;
            } else if(!strcmp(optarg,"heap")) {
                sortType=HEAP;
            } else if(!strcmp(optarg,"shell")) {
                sortType=SHELL;
            } else if(!strcmp(optarg,"combo")) {
                sortType=COMBO;
                insertion_sort_bound=16;
            } else {
                printf("invalid sort type option %s.\n", optarg);
                usage(1, argv[0]);
            }
            break;
          case 'n':
            num_eles = atoi(optarg);
            break;
          default:
            fprintf(stderr, "Unknown option: -%c\n", opt);
            usage(1, argv[0]);
        }
    }

    if (optind < argc) {
        fprintf(stderr, "There are unprocessed parameters left\n");
        usage(1, argv[0]);
    }
    if(filename == NULL) {
        fill_array(num_eles);
    } else {
        num_eles = read_from_file(filename,num_eles);
    }
    printf("Pre-sorted array, num_eles: %d: [", num_eles);
    for(indx=0; indx < num_eles; indx++)
    {
        printf("%d, ", my_array[indx]);
    }
    printf("]\n");
    switch(sortType) {
      case BUBBLE: printf("BubbleSort\n"); BubbleSort(my_array,cmpfun,num_eles);
        break;
      case SELECTION: printf("SelectionSort\n"); SelectionSort(my_array,cmpfun,num_eles);
        break;
      case INSERTION: printf("InsertionSort\n"); InsertionSort(my_array,cmpfun,num_eles);
        break;
      case QUICK: printf("QuickSort\n"); QuickSort(my_array,cmpfun,0, num_eles -1);
      break;
      case MERGE: printf("MergeSort\n"); MergeSort(my_array,cmpfun,num_eles);
        break;
      case HEAP: printf("HeapSort\n"); HeapSort(my_array,cmpfun,num_eles);
        break;
      case SHELL: printf("ShellSort\n"); ShellSort(my_array,cmpfun,num_eles);
        break;
      case COMBO: printf("ComboSort\n"); ComboSort(my_array,cmpfun,0,num_eles-1);
        break;
    }

    printf("Post-sorted array: [");
    for (indx=0; indx < num_eles -1; indx++)
    {
        printf("%d, ",my_array[indx]); 
        if (my_array[indx] > my_array[indx+1])
        {
            printf("bad sort\n");
            return(1);
        }
    }
    printf("%d]\n", my_array[indx]);
    
    printf("Sort successful\n");

    return(0);
}
 
