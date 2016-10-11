// #run: (d-test-fontification)

const class C
{
	int getFoo() const { return 42; }

	const void bar()
	{
		auto foo = getFoo();
		const baz = getFoo();
	}

	const auto foo = 4;
	immutable auto boo = 4;
}
