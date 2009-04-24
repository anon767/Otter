import java.io.*;
import java.util.*;
import java.util.zip.*;

public class SymbolicOutputProcessor
{
	private int maxConfig = 20;
	private int total = 0;
	private int completed = 0;
	private int[] numbOfConf = new int[maxConfig];
	private HashSet<String> unique = new HashSet<String>();
	private HashMap<String, Integer> configurations = new HashMap<String, Integer>();
	
	public final String directory;
	// subdir data contains all the log files
	// subdir result will contain all the html report, but ignore that for now
	
	public SymbolicOutputProcessor(String dir) {
		directory = dir;
	}

	public void processSymexeOutput(String filename)
	{
		try
		{
			// process the zipped output files
			ZipInputStream outputfiles = new ZipInputStream(new FileInputStream(filename));
			ZipEntry zippedOutput;
			
			while ((zippedOutput = outputfiles.getNextEntry()) != null)
			{
				String name = zippedOutput.getName();
				
				if (name.endsWith("-test"))
				{
					BufferedReader br = new BufferedReader(new InputStreamReader(outputfiles));
					String lineread = "";
					int num = 0;
					String application = null;
					String testcase = null;
					boolean coverageStarts = false;
					
					total++;
					while ((lineread = br.readLine()) != null)
					{
						if (lineread.startsWith("command:"))
						{
							application = lineread.substring(lineread.indexOf("application="), lineread.indexOf("testcase="));
							testcase = lineread.substring(lineread.indexOf("testcase="), lineread.length() - 1);
							
							System.out.println(application + " " + testcase);
						}
						else if (lineread.startsWith("It ran for")) {
								System.out.println("Running time: " + lineread.substring(lineread.indexOf("total") + 6, lineread.indexOf("execution") - 1));
						}
						else if (lineread.indexOf("ran to completion;") != -1)
						{
							completed++;
							System.out.println(lineread);
						}
						else if (lineread.indexOf("out of") != -1)
						{
							System.out.println(lineread);
						}
						else if (lineread.indexOf("configurations which covers all") != -1)
						{
							num = Integer.parseInt(lineread.substring(lineread.indexOf("of") + 3, lineread.indexOf("configurations")).trim());
							numbOfConf[num]++;
							System.out.println(lineread.substring(lineread.indexOf('a'), lineread.indexOf("which")) + " needed.");
							
							coverageStarts = true;
						}
						
						if (coverageStarts)
						{
							break;
						}
					}
					
					HashMap<String, int[]> coverage = new HashMap<String, int[]>();
					HashMap<String, int[]> configVal = new HashMap<String, int[]>();
					int config = 0;
					
					while ((lineread = br.readLine()) != null)
					{
						if (lineread.startsWith("Sample value:"))
						{
							String confStr = "";
							
							while ((lineread = br.readLine()) != null)
							{
								if (lineread.indexOf("out of") != -1)
								{
									Integer count = configurations.get(confStr);
									
									if (count == null)
									{
										count = new Integer(0);
									}
									
									count++;
									configurations.put(confStr, count);
									
									break;
								}
								else
								{
									if (lineread.trim().length() > 0)
									{
										int[] hits = configVal.get(lineread);
										
										if (hits == null)
										{
											hits = new int[num];
										}
										
										hits[config] = 1;
										
										configVal.put(lineread, hits);
										
										confStr += lineread.trim() + ",";
									}
								}
							}
						}
						
						if (lineread.startsWith("The lines hit were:"))
						{
							while ((lineread = br.readLine()) != null)
							{
								if (lineread.startsWith("-----"))
								{
									config++;
									
									break;
								}
								else
								{
									if (lineread.trim().length() > 0)
									{
										if (!unique.contains(lineread))
										{
											unique.add(lineread);
										}
										
										int[] hits = coverage.get(lineread);
										
										if (hits == null)
										{
											hits = new int[num];
										}
										
										hits[config] = 1;
										
										coverage.put(lineread, hits);
									}
								}
							}
						}
					}
					
					//calc config vals
//					HashMap<String, String> posConfig = new HashMap<String, String>();
//					Iterator it = configVal.keySet().iterator();
//					
//					while (it.hasNext())
//					{
//						String va = (String)it.next();
//						int[] hits = configVal.get(va);
//						String sig = "";
//						
//						for (int x = 0; x < hits.length; x++)
//						{
//							sig += hits[x];
//						}
//						
//						String vals = posConfig.get(sig);
//						
//						if (vals == null)
//						{
//							vals = "";
//						}
//						
//						vals += va + " ";
//						
//						posConfig.put(sig, vals);
//					}
					
//					BufferedReader sourceReader = new BufferedReader(new FileReader(directory + "data/grep.c"));
//					BufferedWriter reportWriter = new BufferedWriter(new FileWriter(directory + "report/" + testcase.substring(testcase.indexOf('=') + 1) + ".html"));
//					int counter = 1;
//					String key;
//					
//					reportWriter.write("<html>\n");
//					reportWriter.write("<link rel='stylesheet' type='text/css' href='report.css' />\n");
//					reportWriter.write("<head><title>" + application + " " + testcase + "</title></head>\n");
//					reportWriter.write("<body>\n");
//					
//					reportWriter.write("<table>\n");
//					
//					while ((lineread = sourceReader.readLine()) != null)
//					{
//						key = "grep.c:" + counter++;
//						
//						int[] hits = coverage.get(key);
//						
//						if (hits != null)
//						{
//							int hit = 0;
//							String sig = "";
//							for (int x = 0; x < num; x++)
//							{
//								if (hits[x] > 0)
//								{
//									hit++;
//								}
//								sig += hits[x];
//							}
//							
//							if (hit < num)
//							{
//								reportWriter.write("<tr class='important'>\n");
//							}
//							else
//							{
//								reportWriter.write("<tr>\n");
//							}
//							
//							reportWriter.write("<td>" + key + "</td>\n");
//							
//							for (int x = 0; x < num; x++)
//							{
//								reportWriter.write("<td>");
//								
//								if (hits[x] > 0)
//								{
//									reportWriter.write("#");
//								}
//								
//								reportWriter.write("</td>");
//							}
//							
//							reportWriter.write("<td>" + lineread + "</td>\n");
//							
//							if (hit < num)
//							{
//								reportWriter.write("<td>" + posConfig.get(sig) + "</td>\n");
//							}
//							
//							reportWriter.write("</tr>\n");
//						}
//					}
//					
//					reportWriter.write("</table>\n");
//					
//					reportWriter.write("</body>\n");
//					reportWriter.write("</html>\n");
//					
//					sourceReader.close();
//					reportWriter.close();
					
					System.out.println("=======================================");
				}
			}
			
			outputfiles.close();
		}
		catch (IOException ioe)
		{
			ioe.printStackTrace();
		}
	}
	
	public void printStats()
	{
		int totalLines = 4380;
		System.out.println();
		System.out.println(completed + " out of " + total + " testcases finished.");
		System.out.printf("%d out of %d lines were hit (%.2f%%)\n",
											unique.size(), totalLines, 100*unique.size()/(double)totalLines);
		System.out.println();
		for (int x = 0; x < maxConfig; x++)
		{
			if (numbOfConf[x] > 0) {
					System.out.println(numbOfConf[x] + " testcases required " +
														 x + " configurations.");
			}
		}
		
		System.out.println();
		Iterator it = configurations.keySet().iterator();
		while (it.hasNext())
		{
			String confStr = (String)it.next();
			Integer count = configurations.get(confStr);
			
			System.out.println("Config: " + confStr + " appeared " + count + " times.");
		}
	}
	
	public void printCoverage()
	{
		
	}
	
	public static void main(String[] args)
	{
		SymbolicOutputProcessor processor = new SymbolicOutputProcessor(args[0]);
		File dataDir = new File(args[0] + "data/");
		
		String[] fileNames = dataDir.list();
		
		for (int x = 0; x < fileNames.length; x++)
		{
			if (fileNames[x].endsWith(".zip"))
			{
				processor.processSymexeOutput(dataDir.getPath() + "/" + fileNames[x]);
			}
		}
		
		processor.printStats();
	}
}
