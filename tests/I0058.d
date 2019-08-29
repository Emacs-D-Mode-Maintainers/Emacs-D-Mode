// #run: (d-test-indent)

double foo()(double b)
  in // TODO
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
