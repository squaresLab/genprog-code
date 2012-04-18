int gcd(int a, int b) {

  if (a == 0) {
    printf("%d\n", b); 
    return 0;
  } 
  {
    while (b != 0) {
      if (a > b) {
        a = a - b; 
      } else {
        b = b - a; 
      } 
    } 
    printf("%d\n", a); 
  } 

  return 0; 
} 
