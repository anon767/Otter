import java.io.*;
import java.util.*;
import java.util.zip.*;

public class ConcreteOutputProcessor
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

	public ConcreteOutputProcessor(String dir) {
		directory = dir;
	}

	public void processSymexeOutput(String filename,String way)
	{
		System.out.println("File: "+filename);
		try
		{
			// process the zipped output files
			ZipInputStream outputfiles = new ZipInputStream(new FileInputStream(filename));
			ZipEntry zippedOutput;

			while ((zippedOutput = outputfiles.getNextEntry()) != null)
			{
				String name = zippedOutput.getName();
				boolean need_process = true;


				if (name.endsWith("-test"))
				{
					BufferedReader br = new BufferedReader(new InputStreamReader(outputfiles));
					String lineread = "";
					int num = 0;
					String application = null;
					String testcase = null;
					String order = null;
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
						// Guys: change the below 2 lines to suit your need...
						else if (lineread.startsWith("Running Macro")){
							order = lineread.substring(lineread.indexOf("Running Macro -D"), lineread.indexOf(" -DCConf_UID"));
							System.out.println(order);
							if(order.indexOf(way)==-1) {
								need_process = false;
								break;
							}
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
					if(!need_process) {
						System.out.println("*** Not processed.");
						System.out.println("=======================================");
						continue;
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


					System.out.println("=======================================");
				}
			}

			outputfiles.close();
		}
		catch (IOException ioe)
		{
			ioe.printStackTrace();
		}
		catch (Exception e){
			System.out.println(filename);
			e.printStackTrace();
		}
	}

	public void printStats()
	{
		int totalLines = 4421;
		System.out.println();
		System.out.println(completed + " out of " + total + " testcases finished.");
		System.out.printf("%d out of %d lines were hit (%.2f%%)\n", unique.size(), totalLines, 100*unique.size()/(double)totalLines);
		System.out.println();
		for (int x = 0; x < maxConfig; x++)
		{
			if (numbOfConf[x] > 0) {
				System.out.println(numbOfConf[x] + " testcases required " + x + " configurations.");
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

		System.out.println("Line covered:");
		ArrayList<String> list = new ArrayList<String>(unique);
		Collections.sort(list,new Comparator<String>(){
				public int compare(String o1,String o2){
					String[] s1 = o1.split(":");
					String[] s2 = o2.split(":");
					int cmp = s1[0].compareTo(s2[0]);
					if(cmp!=0) return cmp;
					else return Integer.parseInt(s1[1])-Integer.parseInt(s2[1]);
				}
				});
		System.out.println();
		for(String s : list)
			System.out.println(s);

	}


	public static void main(String[] args)
	{
		ConcreteOutputProcessor processor = new ConcreteOutputProcessor(args[0]);
		File dataDir = new File(args[0] + "/");

		String[] fileNames = dataDir.list();

		for (int x = 0; x < fileNames.length; x++)
		{
			if (fileNames[x].endsWith(".zip"))
			{
				processor.processSymexeOutput(dataDir.getPath() + "/" + fileNames[x], args[1]);
			}
		}

		processor.printStats();
	}
}
