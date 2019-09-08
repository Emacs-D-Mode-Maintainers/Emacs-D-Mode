// #min-version: 26.2
// #run: (d-test-fontification)

auto var = true;
enum var = true;
enum int var = true;

enum
{
	a = 1
}
enum : int
{
	a = 1
}
enum Foo
{
	a = 1
}
enum Foo : int
{
	a = 1
}
