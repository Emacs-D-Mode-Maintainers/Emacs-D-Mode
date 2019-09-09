// #run: (d-test-fontification)

void main()
{
	assert(true);

	string[string] aa; string s;
	assert(s in aa);

	aa[s].length;

	run(a ~ b);
}

version(none) string readLink();

static if (true) {} else fun();
static if (true) {} else void fun();

scope(exit) fun();
scope(exit) void fun();
