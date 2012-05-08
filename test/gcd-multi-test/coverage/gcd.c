void *_coverage_fout ;
extern int ( /* missing proto */  printf)() ;
int gcd(int a , int b ) 
{ 

  {
  {
  if (_coverage_fout == 0) {
    {
    _coverage_fout = fopen("/nfs/student/n/neal.holts/Desktop/program_repair/programRepair/genprog-code/trunk/test/gcd-multi-test/coverage/coverage.path",
                           "wb");
    }
  }
  }
  {
  fprintf(_coverage_fout, "12\n");
  fflush(_coverage_fout);
  }
  if (a == 0) {
    {
    fprintf(_coverage_fout, "5\n");
    fflush(_coverage_fout);
    }
    printf("%d\n", b);
  } else {
    {
    fprintf(_coverage_fout, "6\n");
    fflush(_coverage_fout);
    }

  }
  {
  fprintf(_coverage_fout, "13\n");
  fflush(_coverage_fout);
  }
  while (1) {
    {
    fprintf(_coverage_fout, "10\n");
    fflush(_coverage_fout);
    }
    if (b != 0) {
      {
      fprintf(_coverage_fout, "7\n");
      fflush(_coverage_fout);
      }

    } else {
      break;
    }
    {
    fprintf(_coverage_fout, "11\n");
    fflush(_coverage_fout);
    }
    if (a > b) {
      {
      fprintf(_coverage_fout, "8\n");
      fflush(_coverage_fout);
      }
      a -= b;
    } else {
      {
      fprintf(_coverage_fout, "9\n");
      fflush(_coverage_fout);
      }
      b -= a;
    }
  }
  {
  fprintf(_coverage_fout, "14\n");
  fflush(_coverage_fout);
  }
  printf("%d\n", a);
  {
  fprintf(_coverage_fout, "15\n");
  fflush(_coverage_fout);
  }
  return (0);
}
}
