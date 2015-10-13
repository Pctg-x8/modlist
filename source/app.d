import std.stdio;
import std.process, std.path, std.file;
import std.zip, std.conv;
import std.array, std.algorithm, std.range;
import std.datetime;
import javaClassLoader;

void main(string[] args)
{
	immutable dir = buildPath(args.length < 2 ? buildPath(environment["AppData"], ".minecraft") : args[1], "mods");

	writeln("Listing Minecraft Mods in ", dir, "...");
	auto entryList = dir.dirEntries("*.jar", SpanMode.depth);
	size_t foundMods = 0;
	StopWatch sw;
	sw.start();
	foreach(e; entryList)
	{
		// writeln("-- found: ", e.name, ". Extracting jar...");
		scope auto zipEntity = new ZipArchive(read(e.name));
		foreach(amem; zipEntity.directory)
		{
			// writeln("-- found zip member: ", amem.name);
			auto data = zipEntity.expand(amem);
			try
			{
				auto pClass = JavaClass.fromBytes(data);
				if(pClass.hasClassAnnotated)
				{
					foreach(a; pClass.classAnnotations)
					{
						auto annotName = pClass.recursiveFindString(a.type_index);

						if(annotName == "cpw.mods.fml.common.Mod".javaInternalRepresent)
						{
							// Normal Mod
							string name, modid, modversion;
							foreach(i, ev; a.element_value_pairs)
							{
								auto pname = pClass.recursiveFindString(ev.element_name_index);
								if(pname == "name" && ev.value.tag == 's')
								{
									name = pClass.getConstantUtf8Seq(ev.value.const_value_index);
								}
								else if(pname == "modid" && ev.value.tag == 's')
								{
									modid = pClass.getConstantUtf8Seq(ev.value.const_value_index);
								}
								else if(pname == "version" && ev.value.tag == 's')
								{
									modversion = pClass.getConstantUtf8Seq(ev.value.const_value_index);
								}
								/*else
								{
									writeln("------ [Hiding]Parameter #", i + 1, " Name: ", pClass.recursiveFindString(ev.element_name_index));
									writeln("------ [Hiding]Parameter #", i + 1, " Type: ", cast(char)ev.value.tag);
									switch(ev.value.tag)
									{
									case 'B': case 'C': case 'I': case 'S': case 'Z':
										// integer
										writeln("------ [Hiding]Parameter #", i + 1, " Value: ", pClass.getConstantInteger(ev.value.const_value_index));
										break;
									case 'D':
										// double
										writeln("------ [Hiding]Parameter #", i + 1, " Value: ", pClass.getConstantDouble(ev.value.const_value_index));
										break;
									case 'F':
										// float
										writeln("------ [Hiding]Parameter #", i + 1, " Value: ", pClass.getConstantFloat(ev.value.const_value_index));
										break;
									case 'J':
										// long
										writeln("------ [Hiding]Parameter #", i + 1, " Value: ", pClass.getConstantLong(ev.value.const_value_index));
										break;
									case 's':
										// string
										writeln("------ [Hiding]Parameter #", i + 1, " Value: ", pClass.getConstantUtf8Seq(ev.value.const_value_index));
										break;
									default: // none
									}
								}*/
							}
							writeln("Found Mod: ", name, "[", modid, "] ", modversion);
							foundMods++;
						}
						/*else
						{
							// unknown annotation

							writeln("---- Annotation Type Name: ", pClass.recursiveFindString(a.type_index));
							foreach(i, ev; a.element_value_pairs)
							{
								writeln("------ Parameter #", i + 1, " Name: ", pClass.recursiveFindString(ev.element_name_index));
								writeln("------ Parameter #", i + 1, " Type: ", cast(char)ev.value.tag);
								switch(ev.value.tag)
								{
								case 'B': case 'C': case 'I': case 'S': case 'Z':
									// integer
									writeln("------ Parameter #", i + 1, " Value: ", pClass.getConstantInteger(ev.value.const_value_index));
									break;
								case 'D':
									// double
									writeln("------ Parameter #", i + 1, " Value: ", pClass.getConstantDouble(ev.value.const_value_index));
									break;
								case 'F':
									// float
									writeln("------ Parameter #", i + 1, " Value: ", pClass.getConstantFloat(ev.value.const_value_index));
									break;
								case 'J':
									// long
									writeln("------ Parameter #", i + 1, " Value: ", pClass.getConstantLong(ev.value.const_value_index));
									break;
								case 's':
									// string
									writeln("------ Parameter #", i + 1, " Value: ", pClass.getConstantUtf8Seq(ev.value.const_value_index));
									break;
								default: // none
								}
							}
						}*/
					}
				}
			}
			catch(InvalidClassStructureError)
			{
				// writeln("---- Not a class. Ignored.");
			}
		}
	}

	sw.stop();
	writeln(foundMods, " mods found. (", sw.peek.msecs, " ms)");
}
