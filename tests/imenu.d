// #run: (d-test-get-imenu-lines)
// #out: (4 6 11 14 17 19)

void foo(int x) {}

void bar(int x)
{
	return foo(x);
}

extern(C) void _d_throwdwarf(Throwable o)
{}

extern (C) int rt_init()
{}

Duration dur(string units)(long length) @safe pure nothrow @nogc {}

void test(const(char)* str)
{}
