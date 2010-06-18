#include <stdio.h>

int year = 1980;

/*
  int unused(){
    while(1){
      if (year < 1980) ;
      else {
        break;
      }
    }
  }
  */

int isLeapYear(int y){  return (y%4 == 0); }

int main(int argc, char *argv[]) {
  int days;
  days = atoi(argv[1]);

  if (days >= 0){
    while (days > 365)  {
      if (isLeapYear(year)){
          if (days > 366)  {
              days -= 366;
              year += 1;
          }
        /*
	else{
	  printf ("");
	}
        */
      }
      else {
          days -= 365;
          year += 1;
      }
    }
    
  }
  else{
      printf("input days is negative %d\n", days);
      return 0;
  }

  printf("current year is %d\n", year);
  return 0;
}


