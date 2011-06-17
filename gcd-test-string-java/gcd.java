class gcd {  public static void main(String argv[]) { int a,b,c,r1,r2; a = Integer.parseInt(argv[0]); b = Integer.parseInt(argv[1]);
    if (a == 0) {
      System.out.println(b); 
    } 
    while (b != 0) { if (a > b) { a = a - b; } else { b = b - a; } } System.out.println(a); 
    return ;
} }
