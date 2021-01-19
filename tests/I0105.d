// #run: (d-test-fontification)

unittest
{
	assert(absolute("/"[]) == true);
	assert(absolute(""[]) == false);

	version (Windows)
	{
		assert(absolute(r"\"[]) == true);
		assert(absolute(r"\\"[]) == true);
		assert(absolute(r"c:"[]) == true);
	}
}
