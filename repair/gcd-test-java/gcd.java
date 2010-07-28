public class gcd
{
public static void main (String[] args)  
{
double a,b,c;
double r1, r2;
a = Double.parseDouble(args[0]);
b = Double.parseDouble(args[1]);
if (a == 0)    
{
System.out.println((int)b);
}
{
while (b != 0)       
{
if (a > b)        
{
a = a - b;
}
else           
{
b = b - a;
}
}
System.out.println((int)a);
return ;}
}
}
