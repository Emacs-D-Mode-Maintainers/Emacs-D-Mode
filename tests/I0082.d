// #min-version: 24.5
// #run: (d-test-fontification)

struct S
{
	this(Object o) {}
	this(Object* o) {}
	this(Object * o) {}
}
