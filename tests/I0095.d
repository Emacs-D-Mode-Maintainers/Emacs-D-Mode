// #run: (d-test-fontification)
// #min-version: 24.5

alias Type = int;

void foo(const(Type) param);

const(Type) bar(const(Type) param);

const(Type) baz(ref const(Type) param, lazy int param2);

const(Tpl!int) var1;
const(Tpl!(int, "", [1, 2, 3])*) var2;
