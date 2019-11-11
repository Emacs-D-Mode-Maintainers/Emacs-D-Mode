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

@property empty() { return false; }

auto s = "enum Type {}";

assert(a !in b);
if (a.length * b.length) {}
assert(a !is double.nan);
if (a.b in c) {}
write(s ~ "");

invariant {}

class Foo : Bar {}
enum Foo : Bar {}

.TopLevelType var;

debug bool resolveNeeded = false;
