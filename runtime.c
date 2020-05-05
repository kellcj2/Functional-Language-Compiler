#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define TRUE      0x0000000000000006L
#define FALSE     0x0000000000000002L

#define INTEGER_MIN (- (1L << 62))
#define INTEGER_MAX ((1L << 62) -1)

extern int64_t code_entry_point(int64_t, int64_t*) asm("code_entry_point");
void builtin_error() asm("builtin_error");
int64_t print() asm("print");
int64_t tuple_comparison(int64_t*, int64_t*) asm("tuple_comparison");
void print_tuple(int64_t);

void builtin_error(int code) {
  if (code == -1) 
    printf("Error: Invalid operand type\n");
  else if (code == -2)
    printf("Error: Result out of bounds\n");
  else if (code == -3) 
    printf("Error: Invalid 'if' expression type\n");
  else if (code == -4)
    printf("Error: Input out of bounds\n");
  else if (code == -5)
    printf("Error: Input must be 'true', 'false', or an integer\n");
  else if (code == -6)
    printf("Error: Expression must be a tuple\n");
  else if (code == -7)
    printf("Error: Tuple index out of range\n");
  else if (code == -8)
    printf("Error: Comparison operator has different types\n");
  else if (code == -9)
    printf("Error: While loop condition must be boolean\n");
  else if (code == -10)
    printf("Error: Structural equivalent operator has different types\n");
  exit(code);
}

int64_t tuple_comparison(int64_t *t1, int64_t *t2) {
  int64_t *base1 = (int64_t *)t1;
  int64_t size1 = (*base1) >> 1;
  int64_t *base2 = (int64_t *)t2;
  int64_t size2 = (*base2) >> 1;
  if(size1 != size2) return FALSE;
  
  for(int i = 1; i <= size1; i++)
    if(base1[i] != base2[i]) return FALSE;
    
  return TRUE;
}

void print_value(int64_t val) {
  if (val & 1L == 1) {
    if (val < INTEGER_MIN || val > INTEGER_MAX) builtin_error(-2);
    printf("%ld", val >> 1);
  } else if (val == TRUE)
    printf("true");
  else if (val == FALSE)
    printf("false");
  else if ((val & 3L) == 0)
    print_tuple(val);
  else 
    printf("Unknown value: %lx\n", val);
}

void print_tuple(int64_t val) {
    int64_t *base = (int64_t *)val;
    int64_t size = (*base) >> 1;
    //printf("Tuple Size: %ld\n", size);
    printf("(");
    int i;
    for (i = 1; i <= size; i++) {
        printf(" ");
        print_value(base[i]);
    }
    printf(" )");
}

int64_t print(int64_t val) {
    print_value(val);
    printf("\n");
    return val;
}

int main(int argc, char** argv) {
  char *endptr;
  int64_t input = FALSE;
  
  if (argc > 1) {
    if (strcmp(argv[1], "true") == 0)
      input = TRUE;
    else if (strcmp(argv[1], "false") == 0)
      input = FALSE;
    else { 
      endptr = (char*) &argv[1];
      errno = 0;
      int64_t r = strtol(argv[1], &endptr, 10);
      if (*endptr != '\0')
        builtin_error(-5);
      else if (errno || r < INTEGER_MIN || r > INTEGER_MAX )
        builtin_error(-4);
      input = r << 1 | 1;
    }
  }

  int64_t* HEAP = calloc(100000, sizeof(int64_t));
  int64_t result = code_entry_point(input, HEAP);
  print(result);
  return 0;
}
