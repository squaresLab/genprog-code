package gcdp;
import gcdp.printer.*;
public class GCD
{
public static void main (String[] args)      
{
int a = Integer.parseInt(args[0]);
int b = Integer.parseInt(args[1]);
while (a == 0)            
{
Printer.print(Integer.toString(b));
return ;
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
Printer.print(Integer.toString(a));
}
}
}
