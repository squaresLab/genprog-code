import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import soot.PackManager;
import soot.Scene;
import soot.Transform;

public class LineNumbers
{
	public static void main(String[] args)
	{
		run(args[0]);
	}

	public static void run(String classpath)
	{
		System.out.println("ClassPath: " + classpath);

		Scene.v().setSootClassPath(Scene.v().getSootClassPath() + File.pathSeparator + classpath);

		List<String> argsList = new ArrayList<String>();
		argsList.addAll(Arrays.asList(new String[]{"-keep-line-number", "-allow-phantom-refs", "-f", "none",
				"-process-dir", classpath}));
		String[] args = argsList.toArray(new String[0]);

		LineNumberTransformer lnt = new LineNumberTransformer();
		PackManager.v().getPack("jtp").add(new Transform("jtp.myTrans", lnt));

		soot.Main.main(args);
	}
}
