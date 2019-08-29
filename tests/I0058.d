// #run: (d-test-indent)

double foo(double b)
in
  {
    assert(b == b);
  }
out (result)
  {
    assert(result == result);
  }
body
  {
    return b;
  }

double foo (double b)
out (result)
  {
    assert(result == result);
  }
body
  {
    return b;
  }

double foo (double b)
in
  {
    assert(b == b);
  }
body
  {
    return b;
  }
