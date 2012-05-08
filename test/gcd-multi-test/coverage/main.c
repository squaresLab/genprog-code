extern void *_coverage_fout ;
extern int ( /* missing proto */  gcd)() ;
extern int ( /* missing proto */  atoi)() ;
int main(int argc , char **argv ) 
{ int tmp ;
  int tmp___0 ;
  int tmp___1 ;

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
  fprintf(_coverage_fout, "1\n");
  fflush(_coverage_fout);
  }
  tmp = atoi(*(argv + 2));
  {
  fprintf(_coverage_fout, "2\n");
  fflush(_coverage_fout);
  }
  tmp___0 = atoi(*(argv + 1));
  {
  fprintf(_coverage_fout, "3\n");
  fflush(_coverage_fout);
  }
  tmp___1 = gcd(tmp___0, tmp);
  {
  fprintf(_coverage_fout, "4\n");
  fflush(_coverage_fout);
  }
  return (tmp___1);
}
}
