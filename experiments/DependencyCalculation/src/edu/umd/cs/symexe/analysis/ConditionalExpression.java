package edu.umd.cs.symexe.analysis;

import java.util.*;

public class ConditionalExpression
{
	private String conditionalExpression;
	private List<ExecutionPath> trueExecutionPaths;
	private List<ExecutionPath> falseExecutionPaths;
	private List<ExecutionPath> bothExecutionPaths;
	
	public String getConditionalExpression()
	{
		return conditionalExpression;
	}
	public void setConditionalExpression(String conditionalExpression)
	{
		this.conditionalExpression = conditionalExpression;
	}
	
	public boolean addToTrueExecutionPaths(ExecutionPath exePath)
	{
		return trueExecutionPaths.add(exePath);
	}
	
	public boolean addToFalseExecutionPaths(ExecutionPath exePath)
	{
		return falseExecutionPaths.add(exePath);
	}
	
	public boolean addToBothExecutionPaths(ExecutionPath exePath)
	{
		return bothExecutionPaths.add(exePath);
	}
	
	public BitSet getCoveredIfAndOnlyIfTrue()
	{
		// use getCoveredIfTrue(), getCoveredIfFalse(), getNotCoveredIfFalse()
		
		return null;
	}
	
	private BitSet getCoveredIfTrue()
	{
		BitSet covered = new BitSet();
		BitSet coverage = null;
		
		for (int x = 0; x < trueExecutionPaths.size(); x++)
		{
			coverage = trueExecutionPaths.get(x).getCoverage();
			
			covered.or(coverage);
		}
		
		for (int x = 0; x < bothExecutionPaths.size(); x++)
		{
			coverage = bothExecutionPaths.get(x).getCoverage();
			
			covered.or(coverage);
		}
		
		return covered;
	}
	
	private BitSet getCoveredIfFalse()
	{
		BitSet covered = new BitSet();
		BitSet coverage = null;
		
		for (int x = 0; x < falseExecutionPaths.size(); x++)
		{
			coverage = falseExecutionPaths.get(x).getCoverage();
			
			covered.or(coverage);
		}
		
		for (int x = 0; x < bothExecutionPaths.size(); x++)
		{
			coverage = bothExecutionPaths.get(x).getCoverage();
			
			covered.or(coverage);
		}
		
		return covered;
	}
	
	private BitSet getNotCoveredIfTrue()
	{
		return null;	
	}
	
	private BitSet getNotCoveredIfFalse()
	{
		BitSet covered = getCoveredIfFalse();
		
		BitSet notCovered = (BitSet)covered.clone();
		notCovered.flip(0, covered.length());
		
		return notCovered;
	}

	public static void main(String[] args)
	{
	
	}
}
