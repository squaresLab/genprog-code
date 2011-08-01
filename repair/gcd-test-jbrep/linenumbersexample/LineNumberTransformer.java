import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.SootClass;
import soot.SootMethod;
import soot.Unit;
import soot.tagkit.LineNumberTag;

public class LineNumberTransformer extends BodyTransformer
{
	LineNumberTransformer()
	{

	}

	@Override
	protected void internalTransform(Body b, String phaseName, Map options)
	{
		SootClass sc = b.getMethod().getDeclaringClass();
		SootMethod sm = b.getMethod();

		System.out.println("Visiting " + sc.getName() + " : " + sm.getName());

		for (Unit unit : sm.getActiveBody().getUnits())
		{
			int lineNum = getLineNumberForUnit(unit);

			System.out.println(unit + " line num = " + lineNum);
		}
	}

	static int getLineNumberForUnit(Unit unit)
	{
		int line = -1;
		LineNumberTag tag = (LineNumberTag) unit.getTag("LineNumberTag");

		if (tag != null)
		{
			line = tag.getLineNumber() - 1;
		}

		return line;
	}

}