package edu.umd.cs.symexe.analysis;

import java.util.*;

public class ExecutionPath
{
	private List<String> pathConditions;
	private BitSet coverage;
	
	public boolean assumesConditionalExpressionTrue(String conditionalExpression)
	{
		return false;
	}
	
	public boolean assumesConditionalExpressionFalse(String conditionalExpression)
	{
		return false;
	}
	
	public boolean noAssumptionsForCoditionalExpression(String conditionalExpression)
	{
		return false;
	}
	
	public List<String> getPathConditions()
	{
		return pathConditions;
	}
	public void setPathConditions(List<String> pathConditions)
	{
		this.pathConditions = pathConditions;
	}

	public BitSet getCoverage()
	{
		return coverage;
	}
	public void setCoverage(BitSet coverage)
	{
		this.coverage = coverage;
	}

	public static void main(String[] args)
	{
	
	}
}
