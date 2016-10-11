// #run: (d-test-get-imenu-lines)
// #out: (4 6 11 14 17 19 22 25 33 34 37 40 43 46 49 52 54 56)

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

extern (C++) static const(char)* searchPath(Strings* path, const(char)* name, bool cwd)
{}

auto execute(in char[][] args,
             const string[string] env = null,
             Config config = Config.none,
             size_t maxOutput = size_t.max,
             in char[] workDir = null)
    @trusted //TODO: @safe
{}

version(StdDdoc) string readLink(C)(const(C)[] link) @safe;
else version(Posix) string readLink(C)(const(C)[] link) @safe
{}

enum ClockType
{}

enum Foo : int
{}

string absolutePath(string path, lazy string base = getcwd())
{}

private final class C
{}

public struct S
{}

alias xmlParse = parseDocument!XmlParseConfig;

alias parseDocument!XmlParseConfig xmlParse;

void test()
{
	alias foo = bar;
}
