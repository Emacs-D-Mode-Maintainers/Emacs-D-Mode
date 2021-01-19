// #condition: (version<= "26.1" emacs-version)
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
