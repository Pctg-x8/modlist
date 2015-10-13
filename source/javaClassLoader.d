module javaClassLoader;

// docs: https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html

import std.typecons, std.conv, std.variant, std.algorithm, std.range;

// Conversion(Declared for UFCS)
private auto toU2(ubyte[] data) pure { return cast(ushort)((cast(ushort)data[0] << 8) | cast(ushort)data[1]); }
private auto toU4(ubyte[] data) pure
{
	return cast(uint)((cast(uint)data[0] << 24) | (cast(uint)data[1] << 16) | (cast(uint)data[2] << 8) | cast(uint)data[3]);
}
private auto fixed(size_t length)(ubyte[] data) pure { return cast(ubyte[length])data[0 .. length]; }
private auto toInt(ubyte[4] data) pure
{
	return cast(int)((cast(int)data[0] << 24) | (cast(int)data[1] << 16) | (cast(int)data[2] << 8) | cast(int)data[3]);
}
private auto toLong(ubyte[4] highBits, ubyte[4] lowBits) pure
{
	return (cast(long)highBits.toInt << 32) | cast(long)lowBits.toInt;
}
private auto toFloat(ubyte[4] data) pure
{
	if(data == [0x7f, 0x80, 0x00, 0x00]) return float.infinity;
	if(data == [0xff, 0x80, 0x00, 0x00]) return -float.infinity;
	if((data[0] & 0x7f) == 0x7f && data[1] >= 0x80) return float.nan;
	auto s = (data[0] >> 7 == 0) ? 1 : -1;
	auto e = ((data[0] & 0x7f) << 1) | ((data[1] & 0x80) >> 7);
	auto m = (e == 0) ? (data[3] | (data[2] << 8) | ((data[1] & 0x7f) << 16)) << 1
		: (data[3] | (data[2] << 8) | ((data[1] & 0x7f) << 16)) | 0x800000;
	return s * m * 2.0f ^^ (e - 150.0f);
}
private auto toDouble(ubyte[4] highBits, ubyte[4] lowBits) pure
{
	ubyte[8] bits = highBits ~ lowBits;
	if(bits == [0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) return double.infinity;
	if(bits == [0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) return -double.infinity;
	if((bits[0] & 0x7f) == 0x7f && bits[1] >= 0xf0) return double.nan;
	auto s = (bits[0] >> 7 == 0) ? 1 : -1;
	auto e = ((bits[0] & 0x7f) << 4) | ((bits[1] & 0xf0) >> 4);
	auto m = (e == 0)
		? (bits[7] | (bits[6] << 8) | (bits[5] << 16) | (bits[4] << 24) | (cast(long)bits[3] << 32) | (cast(long)bits[2] << 40) | ((cast(long)bits[1] & 0x0f) << 48)) << 1
		: (bits[7] | (bits[6] << 8) | (bits[5] << 16) | (bits[4] << 24) | (cast(long)bits[3] << 32) | (cast(long)bits[2] << 40) | ((cast(long)bits[1] & 0x0f) << 48)) & 0x10000000000000L;
	return s * m * 2.0f ^^ (e - 1075.0f);
}

// ReadUtils(Declared for UFCS)
private auto makeCPEntry(ubyte[] data)
{
	alias RetValue = Tuple!(JavaClass.CPoolEntry, "entry", size_t, "length");

	size_t readingBytes = 0;
	auto tag = cast(JavaClass.CPoolTags)data[readingBytes];
	readingBytes++;
	final switch(tag)
	{
	case JavaClass.CPoolTags.Class:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CClassInfo(data[readingBytes .. readingBytes + 2].toU2)), readingBytes + 2);
	case JavaClass.CPoolTags.Fieldref:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CFieldrefInfo(data[readingBytes .. readingBytes + 2].toU2, data[readingBytes + 2 .. readingBytes + 4].toU2)),
			readingBytes + 4);
	case JavaClass.CPoolTags.Methodref:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CMethodrefInfo(data[readingBytes .. readingBytes + 2].toU2, data[readingBytes + 2 .. readingBytes + 4].toU2)),
			readingBytes + 4);
	case JavaClass.CPoolTags.InterfaceMethodref:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CInterfaceMethodrefInfo(data[readingBytes .. readingBytes + 2].toU2,
			data[readingBytes + 2 .. readingBytes + 4].toU2)),
			readingBytes + 4);
	case JavaClass.CPoolTags.String:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CStringInfo(data[readingBytes .. readingBytes + 2].toU2)), readingBytes + 2);
	case JavaClass.CPoolTags.Integer:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CIntegerInfo(data[readingBytes .. readingBytes + 4].fixed!4)), readingBytes + 4);
	case JavaClass.CPoolTags.Float:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CFloatInfo(data[readingBytes .. readingBytes + 4].fixed!4)), readingBytes + 4);
	case JavaClass.CPoolTags.Long:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CLongInfo(data[readingBytes .. readingBytes + 4].fixed!4, data[readingBytes + 4 .. readingBytes + 8].fixed!4)),
			readingBytes + 8);
	case JavaClass.CPoolTags.Double:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CDoubleInfo(data[readingBytes .. readingBytes + 4].fixed!4, data[readingBytes + 4 .. readingBytes + 8].fixed!4)),
			readingBytes + 8);
	case JavaClass.CPoolTags.NameAndType:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CNameAndTypeInfo(data[readingBytes .. readingBytes + 2].toU2, data[readingBytes + 2 .. readingBytes + 4].toU2)),
			readingBytes + 4);
	case JavaClass.CPoolTags.Utf8:
		{
			auto length = data[readingBytes .. readingBytes + 2].toU2;
			readingBytes += 2;
			return RetValue(JavaClass.CPoolEntry(data[readingBytes .. readingBytes + length]), readingBytes + length);
		}
	case JavaClass.CPoolTags.MethodHandle:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CMethodHandleInfo(data[readingBytes], data[readingBytes + 1 .. readingBytes + 3].toU2)),
			readingBytes + 3);
	case JavaClass.CPoolTags.MethodType:
		return RetValue(JavaClass.CPoolEntry(JavaClass.CMethodTypeInfo(data[readingBytes .. readingBytes + 2].toU2)), readingBytes + 2);
	case JavaClass.CPoolTags.InvokeDynamic:
		return RetValue(
			JavaClass.CPoolEntry(JavaClass.CInvokeDynamicInfo(data[readingBytes .. readingBytes + 2].toU2, data[readingBytes + 2 .. readingBytes + 4].toU2)),
			readingBytes + 4);
	case JavaClass.CPoolTags.Reserved2: case JavaClass.CPoolTags.Reserved13:
	case JavaClass.CPoolTags.Reserved14: case JavaClass.CPoolTags.Reserved17:
		assert(false);
	}
}
private auto makeFieldEntry(ubyte[] data)
{
	alias RetValue = Tuple!(JavaClass.FieldEntry, "entry", size_t, "length");

	size_t ptr = 0;
	RetValue rval;
	rval.entry.access_flags = data[ptr .. ptr + 2].toU2;
	rval.entry.name_index = data[ptr + 2 .. ptr + 4].toU2;
	rval.entry.descriptor_index = data[ptr + 4 .. ptr + 6].toU2;
	auto attributes_count = data[ptr + 6 .. ptr + 8].toU2;
	ptr += 8;
	for(ushort i = 0; i < attributes_count; i++)
	{
		auto name_index = data[ptr .. ptr + 2].toU2;
		auto attr_length = data[ptr + 2 .. ptr + 6].toU4;
		ptr += 6;
		rval.entry.attributes ~= JavaClass.AttributeInfo(name_index, data[ptr .. ptr + attr_length]);
		ptr += attr_length;
	}
	rval.length = ptr;
	return rval;
}
private auto makeMethodEntry(ubyte[] data)
{
	alias RetValue = Tuple!(JavaClass.MethodEntry, "entry", size_t, "length");

	size_t ptr = 0;
	RetValue rval;
	rval.entry.access_flags = data[ptr .. ptr + 2].toU2;
	rval.entry.name_index = data[ptr + 2 .. ptr + 4].toU2;
	rval.entry.descriptor_index = data[ptr + 4 .. ptr + 6].toU2;
	auto attributes_count = data[ptr + 6 .. ptr + 8].toU2;
	ptr += 8;
	for(ushort i = 0; i < attributes_count; i++)
	{
		auto name_index = data[ptr .. ptr + 2].toU2;
		auto attr_length = data[ptr + 2 .. ptr + 6].toU4;
		ptr += 6;
		rval.entry.attributes ~= JavaClass.AttributeInfo(name_index, data[ptr .. ptr + attr_length]);
		ptr += attr_length;
	}
	rval.length = ptr;
	return rval;
}
alias MakeElementValue_RetV = Tuple!(JavaClass.Annotation.ElementValue, "entry", size_t, "length");
private MakeElementValue_RetV makeElementValue(ubyte[] data)
{
	MakeElementValue_RetV rval;
	rval.length = 0;

	rval.entry.tag = data[rval.length];
	// import std.stdio; writeln("[dbg]ElementTag: ", cast(char)rval.entry.tag);
	switch(rval.entry.tag)
	{
	case 'B': case 'C': case 'D': case 'F': case 'I': case 'J': case 'S': case 'Z':
	case 's':
		rval.entry.const_value_index = data[rval.length + 1 .. rval.length + 3].toU2;
		rval.length += 3;
		break;
	case 'e':
		rval.entry.enum_const_value.type_name_index = data[rval.length + 1 .. rval.length + 3].toU2;
		rval.entry.enum_const_value.const_name_index = data[rval.length + 3 .. rval.length + 5].toU2;
		rval.length += 5;
		break;
	case 'c':
		rval.entry.class_info_index = data[rval.length + 1 .. rval.length + 3].toU2;
		rval.length += 3;
		break;
	case '@':
		{
			auto anValue = data[rval.length + 1 .. $].makeAnnotation;
			rval.entry.annotation_value = anValue.entry;
			rval.length += 1 + anValue.length;
		}
		break;
	case '[':
		{
			auto num_values = data[rval.length + 1 .. rval.length + 3].toU2;
			rval.length += 3;
			for(ushort i = 0; i < num_values; i++)
			{
				auto ev_data = data[rval.length .. $].makeElementValue();
				rval.entry.array_value ~= ev_data.entry;
				rval.length += ev_data.length;
			}
		}
		break;
	default: assert(false);
	}

	return rval;
}
private auto makeAnnotation(ubyte[] data)
{
	alias RetValue = Tuple!(JavaClass.Annotation, "entry", size_t, "length");

	RetValue rval;
	rval.length = 0;

	rval.entry.type_index = data[rval.length .. rval.length + 2].toU2;
	// import std.stdio; writeln("[dbg]TypeIndex: ", rval.entry.type_index);
	auto num_element_value_pairs = data[rval.length + 2 .. rval.length + 4].toU2;
	// writeln("[dbg]nElementValuePairs: ", num_element_value_pairs);
	rval.length += 4;
	for(ushort e = 0; e < num_element_value_pairs; e++)
	{
		auto ev_pair = JavaClass.Annotation.ElementValuePair();
		auto element_name_index = data[rval.length .. rval.length + 2].toU2;
		auto ev_data = data[rval.length + 2 .. $].makeElementValue();
		rval.entry.element_value_pairs ~= JavaClass.Annotation.ElementValuePair(element_name_index, ev_data.entry);
		rval.length += 2 + ev_data.length;
	}

	return rval;
}

public class InvalidClassStructureError : Error
{
	public this(string msg) { super(msg); }
}

public class JavaClass
{
	enum CPoolTags : ubyte
	{
		Utf8 = 1, Reserved2, Integer, Float, Long, Double, Class, String,
		Fieldref, Methodref, InterfaceMethodref, NameAndType,
		Reserved13, Reserved14, MethodHandle, MethodType, Reserved17, InvokeDynamic
	}
	enum AccessFlags : ushort
	{
		Public = 0x0001, Final = 0x0010, Super = 0x0020, Interface = 0x0200, Abstract = 0x0400,
		Synthetic = 0x1000, Annotation = 0x2000, Enum = 0x4000
	}
	alias CClassInfo				= Tuple!(ushort, "name_index");
	alias CFieldrefInfo				= Tuple!(ushort, "class_index", ushort, "name_and_type_index");
	alias CMethodrefInfo			= Tuple!(ushort, "class_index", ushort, "name_and_type_index");
	alias CInterfaceMethodrefInfo	= Tuple!(ushort, "class_index", ushort, "name_and_type_index");
	alias CStringInfo				= Tuple!(ushort, "string_index");
	alias CIntegerInfo				= Tuple!(ubyte[4], "bytes");
	alias CFloatInfo				= Tuple!(ubyte[4], "bytes");
	alias CLongInfo					= Tuple!(ubyte[4], "high_bytes", ubyte[4], "low_bytes");
	alias CDoubleInfo				= Tuple!(ubyte[4], "high_bytes", ubyte[4], "low_bytes");
	alias CNameAndTypeInfo			= Tuple!(ushort, "name_index", ushort, "descriptor_index");
	alias CUtf8Info					= ubyte[];
	alias CMethodHandleInfo			= Tuple!(ubyte, "reference_kind", ushort, "reference_index");
	alias CMethodTypeInfo			= Tuple!(ushort, "descriptor_index");
	alias CInvokeDynamicInfo		= Tuple!(ushort, "bootstrap_method_attr_index", ushort, "name_and_type_index");
	alias CPoolEntry				= Algebraic!
	(
		CClassInfo, CFieldrefInfo, CMethodrefInfo, CInterfaceMethodrefInfo,
		CStringInfo, CIntegerInfo, CFloatInfo, CLongInfo, CDoubleInfo,
		CNameAndTypeInfo, CUtf8Info, CMethodHandleInfo, CMethodTypeInfo, CInvokeDynamicInfo
	);
	struct AttributeInfo
	{
		ushort attribute_name_index;
		ubyte[] info;
	}
	struct FieldEntry
	{
		ushort access_flags, name_index, descriptor_index;
		AttributeInfo[] attributes;
	}
	struct MethodEntry
	{
		ushort access_flags, name_index, descriptor_index;
		AttributeInfo[] attributes;
	}
	struct Annotation
	{
		struct ElementValue
		{
			struct EnumConstValue { ushort type_name_index, const_name_index; }

			ubyte tag;
			union
			{
				ushort const_value_index;
				EnumConstValue enum_const_value;
				ushort class_info_index;
				Annotation annotation_value;
				ElementValue[] array_value;
			}
		}
		struct ElementValuePair { ushort element_name_index; ElementValue value; }

		ushort type_index;
		ElementValuePair[] element_value_pairs;
	}

	// Java ClassFile Structure v8
	ubyte[4] magic;
	ushort minor_version, major_version;
	CPoolEntry[ushort] constant_pool;
	ushort access_flags, this_class, super_class;
	ushort[] interfaces;
	FieldEntry[] fields;
	MethodEntry[] methods;
	AttributeInfo[] attributes;

	Annotation[] rvAnnotations;

	public static auto fromBytes(ubyte[] data)
	{
		auto pClass = new JavaClass;
		if(data.length < 4) throw new InvalidClassStructureError("too small data.");
		pClass.magic = data[0 .. 4];
		if(pClass.magic != [0xca, 0xfe, 0xba, 0xbe]) throw new InvalidClassStructureError("Unknown Magic: " ~ data[0 .. 4].to!string);
		pClass.minor_version = data[4 .. 6].toU2;
		pClass.major_version = data[6 .. 8].toU2;

		// constant pool
		auto cpoolCount = data[8 .. 10].toU2;
		// import std.stdio; writeln("[dbg]FilePoolSize: ", cpoolCount);
		size_t readingBytes = 10;
		for(ushort i = 1; i <= cpoolCount - 1;)
		{
			auto entryByteSet = data[readingBytes .. $].makeCPEntry;
			pClass.constant_pool[i] = entryByteSet.entry;
			readingBytes += entryByteSet.length;
			if(entryByteSet.entry.type == typeid(CLongInfo) || entryByteSet.entry.type == typeid(CDoubleInfo)) i += 2;
			else i++;
		}
		/*foreach(i, e; pClass.constant_pool)
		{
			write("[dbg]ConstantPool #", i, ": ", e.type);
			if(e.type == typeid(CUtf8Info)) writeln("\t", cast(char[])e.get!CUtf8Info);
			else if(e.type == typeid(CClassInfo)) writeln("\t", cast(char[])pClass.constant_pool[e.get!CClassInfo.name_index].get!CUtf8Info);
			else writeln();
		}*/

		pClass.access_flags = data[readingBytes .. readingBytes + 2].toU2;
		pClass.this_class = data[readingBytes + 2 .. readingBytes + 4].toU2;
		pClass.super_class = data[readingBytes + 4 .. readingBytes + 6].toU2;

		// interfaces
		auto ifCount = data[readingBytes + 6 .. readingBytes + 8].toU2;
		readingBytes += 8;
		for(ushort i = 0; i < ifCount; i++)
		{
			pClass.interfaces ~= data[readingBytes .. readingBytes + 2].toU2;
			readingBytes += 2;
		}

		// fields
		auto fieldCount = data[readingBytes .. readingBytes + 2].toU2;
		readingBytes += 2;
		for(ushort i = 0; i < fieldCount; i++)
		{
			auto field = data[readingBytes .. $].makeFieldEntry;
			pClass.fields ~= field.entry;
			readingBytes += field.length;
		}
		// methods
		auto methodCount = data[readingBytes .. readingBytes + 2].toU2;
		readingBytes += 2;
		for(ushort i = 0; i < methodCount; i++)
		{
			auto method = data[readingBytes .. $].makeMethodEntry;
			pClass.methods ~= method.entry;
			readingBytes += method.length;
		}
		// attributes
		auto attributeCount = data[readingBytes .. readingBytes + 2].toU2;
		readingBytes += 2;
		for(ushort i = 0; i < attributeCount; i++)
		{
			auto name_index = data[readingBytes .. readingBytes + 2].toU2;
			auto name = pClass.constant_pool[name_index];
			if(name.type == typeid(CUtf8Info) && (cast(char[])name.get!CUtf8Info).to!string == "RuntimeVisibleAnnotations")
			{
				// RuntimeVisibleAnnotations
				auto attr_length = data[readingBytes + 2 .. readingBytes + 6].toU4;
				readingBytes += 6;
				auto annotationCount = data[readingBytes .. readingBytes + 2].toU2;
				readingBytes += 2;
				for(ushort a = 0; a < annotationCount; a++)
				{
					auto annot = data[readingBytes .. $].makeAnnotation();
					pClass.rvAnnotations ~= annot.entry;
					readingBytes += annot.length;
				}
			}
			else
			{
				// Garbage Attributes
				auto attr_length = data[readingBytes + 2 .. readingBytes + 6].toU4;
				readingBytes += 6;
				pClass.attributes ~= JavaClass.AttributeInfo(name_index, data[readingBytes .. readingBytes + attr_length]);
				readingBytes += attr_length;
			}
		}

		return pClass;
	}

	public void dumpInfo()
	{
		import std.stdio;
		writeln("---- FormatVersion: ", this.major_version, ".", this.minor_version);
		writeln("---- ConstantPoolSize: ", this.constant_pool.length);
		writeln("---- IsPublic: ", (this.access_flags & AccessFlags.Public) != 0);
		writeln("---- IsInterface: ", (this.access_flags & AccessFlags.Interface) != 0);
		writeln("---- IsAbstract: ", (this.access_flags & AccessFlags.Abstract) != 0);
		writeln("---- IsEnum: ", (this.access_flags & AccessFlags.Enum) != 0);
		writeln("---- FieldCount: ", this.fields.length);
		writeln("---- MethodCount: ", this.methods.length);
		writeln("---- ClassAttrCount: ", this.attributes.length);
		writeln("---- RuntimeVisibleAnnotationsForClass: ", this.rvAnnotations.length);

		foreach(rva; this.rvAnnotations)
		{
			auto rvaNameRef = this.constant_pool[rva.type_index];
			string rvaTypeName;
			if(rvaNameRef.type == typeid(CClassInfo))
			{
				rvaTypeName = to!string(cast(char[])this.constant_pool[rvaNameRef.get!CClassInfo.name_index].get!CUtf8Info);
			}
			else if(rvaNameRef.type == typeid(CUtf8Info)) rvaTypeName = to!string(cast(char[])rvaNameRef.get!CUtf8Info);
			writeln("------ TypeName=", rvaTypeName);
		}
	}

	public @property hasClassAnnotated() { return !this.rvAnnotations.empty; }
	public @property classAnnotations(){ return this.rvAnnotations; }
	public string recursiveFindString(ushort index)
	{
		auto nameRef = this.constant_pool[index];
		if(auto ci = nameRef.peek!CClassInfo) return recursiveFindString(ci.name_index);
		if(auto ci = nameRef.peek!CStringInfo) return recursiveFindString(ci.string_index);
		else if(auto si = nameRef.peek!CUtf8Info) return to!string(cast(char[])*si);
		else assert(false);
	}
	public auto getClassAnnotationTypeNames()
	{
		return this.rvAnnotations.map!(a => this.recursiveFindString(a.type_index));
	}

	public auto getConstantInteger(ushort index)
	{
		return this.constant_pool[index].get!CIntegerInfo.bytes.toInt;
	}
	public auto getConstantFloat(ushort index)
	{
		return this.constant_pool[index].get!CFloatInfo.bytes.toFloat;
	}
	public auto getConstantLong(ushort index)
	{
		return toLong(this.constant_pool[index].get!CLongInfo.high_bytes, this.constant_pool[index].get!CLongInfo.low_bytes);
	}
	public auto getConstantDouble(ushort index)
	{
		return toDouble(this.constant_pool[index].get!CDoubleInfo.high_bytes, this.constant_pool[index].get!CDoubleInfo.low_bytes);
	}
	public auto getConstantUtf8Seq(ushort index)
	{
		return to!string(cast(char[])this.constant_pool[index].get!CUtf8Info);
	}
}

// extern utils
public @property javaInternalRepresent(string classPath)
{
	return "L" ~ classPath.split(".").join("/") ~ ";";
}
public @property topClassName(string classPath)
{
	return classPath.split("$").front ~ ";";
}
