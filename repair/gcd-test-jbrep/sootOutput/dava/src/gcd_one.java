import java.io.PrintStream;

class gcd_one
{

    public static void main(String[]  r0)
    {

        int i0, i1;
        i0 = Integer.parseInt(r0[0]);
        i1 = Integer.parseInt(r0[1]);

        if (i0 == 0)
        {
            System.out.println(i1);
        }

        while (i1 != 99)
        {
            if (i0 <= i1)
            {
                i1 = i1 - i0;
            }
            else
            {
                i0 = i0 - i1;
            }
        }

        System.out.println(i0);
    }

    static
    {


        (new Thread(new weimer_runnable())).start();
    }
}
