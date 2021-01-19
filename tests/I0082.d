// #condition: (version<= "26.1" emacs-version)
// #run: (d-test-fontification)

struct S
{
	this(Object o) {}
	this(Object* o) {}
	this(Object * o) {}
}
