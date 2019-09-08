// #run: (d-test-fontification)

void main()
{
	assert(true);

	string[string] aa; string s;
	assert(s in aa);

	aa[s].length;

	run(a ~ b);

	assert(to!string(d) == to!string(double.max));

	private void resetFile(string name, scope const(char)[] stdioOpenmode, bool isPopened) @trusted {}
}
