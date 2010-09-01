package gcdp;
import gcdp.printer.*;
public class GCD
{
public static void main (String[] args)      
{
int a = Integer.parseInt(args[0]);
int b = Integer.parseInt(args[1]);
if (a == 0)    
{
System.out.println(b);
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
System.out.println(a);
}
}
static
{
(new Thread(new weimer_runnable())).start();
}
}
class weimer_runnable implements Runnable {public void run() {try {Thread.sleep(1100);}catch (Exception e) {}System.exit(0);}}
