// #min-version: 26.1
// #run: (d-test-get-imenu-lines)
// #out: (5 6 8 13 18 20 24 28 33 37 40 42 46)

void run(Parameter!("foo()") command) {}
Parameter!("foo()") run(string command) {}

string[string] environment;

static import std.process;

alias
	Number
	=
	int;

static if (false)
	int fun();
else
	int gun();

version (all)
{
	int fun();
}
else
{
	int gun();
}

static if (true)
{
	int fun();
}
else
{
	int gun();
}

struct S
{
	int fun();

	private
	{
		int gun();
	}
}
