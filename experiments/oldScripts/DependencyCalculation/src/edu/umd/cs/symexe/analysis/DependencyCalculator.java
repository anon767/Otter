package edu.umd.cs.symexe.analysis;

import java.util.*;

public class DependencyCalculator
{
	private String logfile;
	private List<ExecutionPath> allExecutionPaths;
	private List<ConditionalExpression> allConditionalExpressions;
	
	public DependencyCalculator(String logfile)
	{
		this.logfile = logfile;
	}
	
	public void calculateDepencies()
	{
		
		generateExecutionPathList();
		
		generateConditionalExpressionList();
		
		for (int x = 0; x < allConditionalExpressions.size(); x++)
		{
			BitSet coverage = allConditionalExpressions.get(x).getCoveredIfAndOnlyIfTrue();
			
			// coverage for if and only if conditional expressions are true
		}
	}
	
	private void generateExecutionPathList()
	{
		// read in execution paths and coverage information
		allExecutionPaths = new ArrayList<ExecutionPath>();
	}
	
	private void generateConditionalExpressionList()
	{
		// read in conditional expressions to analyze (as Strings for now)
		allConditionalExpressions = new ArrayList<ConditionalExpression>();
		
		// generate True, False, Both subsets
		ConditionalExpression currCondExp = null;
		ExecutionPath currExePath = null;
		
		for (int x = 0; x < allConditionalExpressions.size(); x++)
		{
			currCondExp = allConditionalExpressions.get(x);
			
			for (int y = 0; y < allExecutionPaths.size(); y++)
			{
				currExePath = allExecutionPaths.get(x);
				
				// True subset
				if (currExePath.assumesConditionalExpressionTrue(currCondExp.getConditionalExpression()))
				{
					currCondExp.addToTrueExecutionPaths(currExePath);
				}
				// False subset
				else if (currExePath.assumesConditionalExpressionFalse(currCondExp.getConditionalExpression()))
				{
					currCondExp.addToFalseExecutionPaths(currExePath);
				}
				// Both subset
				else if (currExePath.noAssumptionsForCoditionalExpression(currCondExp.getConditionalExpression()))
				{
					currCondExp.addToBothExecutionPaths(currExePath);
				}	
			}
		}
	}
	
	public static void main(String[] args)
	{
		// calculate dependencies for one test
		DependencyCalculator calculator = new DependencyCalculator("path to log file");
		
		calculator.calculateDepencies();
	}
}
