// #min-version: 24.5
// #run: (d-test-indent)

void foo(T)(T stuff)
if (isInputRange!T) {
}

struct S
{
  void foo()()
  if (true)
    {
    }

  this()()
  if (true)
    {
    }
}
