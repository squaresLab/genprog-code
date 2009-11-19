// this is a toy program, it is "working" whenever the value of top is
// greater than it's argument
int main(int argc, char *argv[]) {
  int top = 10;
  int bottom = atoi(argv[1]);
  
  if (top > bottom)
    printf("success\n"); 
  
  return 0; 
} 
