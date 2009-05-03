package edu.umd.cs.symexe.analysis;

import java.util.*;

public class ConditionalExpression
{
	private String expression;
	private List<ExecutionPath> TrueExecutionPaths;
	private List<ExecutionPath> FalseExecutionPaths;
	private List<ExecutionPath> BothExecutionPaths;
	
	public BitSet getCoveredWhenTrue()
	{
		return null;
	}
	
	public BitSet getCoveredWhenFalse()
	{
		return null;
	}
	
	public BitSet getNotCoveredWhenTrue()
	{
		return null;	
	}
	
	public BitSet getNotCoveredWhenFalse()
	{
		return null;	
	}
	
	public static void main(String[] args)
	{
	
	}
}
