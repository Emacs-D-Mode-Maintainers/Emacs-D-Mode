// #min-version: 26.1
// #run: (d-test-fontification)

auto dg = (Object a, Object b) {};
alias dg2 = (a, b) {};

alias dg3 = a => a + 1;
alias dg4 = fun(a => a + 1);
alias dg5 = fun(a, b => b + 1);

void fun()
{
	foreach (a; c) foo();
	foreach (a, b; c) foo();
	foreach (a; c) {}
	foreach (a, b; c) {}

	try {} catch (Exception e) {}
	try {} catch (Exception) {}
	try {} catch (Exception e) run();
	try {} catch (Exception) run();
}
