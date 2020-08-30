// #min-version: 26.1
// #run: (d-test-fontification)

struct S
{
	this(Object o) {}
	this(Object* o) {}
	this(Object * o) {}
}
