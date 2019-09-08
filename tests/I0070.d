// #min-version: 24.5
// #run: (progn (c-set-offset 'func-decl-cont #'d-lineup-arglists) (d-test-indent))

auto foo(A, B, C)
        (A a, B b, C c)
{
}
