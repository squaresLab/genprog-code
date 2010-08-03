class gcd
{
public static void main (String[] args)  
{
int a = Integer.parseInt(args[0]);
int b = Integer.parseInt(args[1]);
if (a == 0)    
{
System.out.println(b);
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
System.out.println(a);
}
}
}
