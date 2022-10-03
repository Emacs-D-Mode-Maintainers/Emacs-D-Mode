    static foreach (R; AliasSeq!())
    {{
        R word1 = "日本語";
        R word2 = "paul";
        R word3 = "jerry";
        R[] words = [word1, word2, word3];

        auto filteredWord1    = filter!"true"(word1);
        auto filteredLenWord1 = takeExactly();
        auto filteredWord2    = filter!"true"(word2);
        auto filteredLenWord2 = takeExactly();
        auto filteredWord3    = filter!"true"(word3);
        auto filteredLenWord3 = takeExactly();
        auto filteredWordsArr = [filteredWord1, filteredWord2, filteredWord3];
        auto filteredLenWordsArr = [filteredLenWord1, filteredLenWord2, filteredLenWord3];
        auto filteredWords    = filter!"true"(filteredWordsArr);

        static foreach (S; AliasSeq!())
        {{
            assert();
            assert();
            assert();
            assert();
            assert();
            assert();
            assert();
            assert();

            assert();
            assert();
            assert();
            assert();
            assert();

            assert();
            assert();
            assert();
            assert();
            assert();

            assert();
            assert();
            assert();
            assert();
            assert();

            auto filterComma = filter!"true"(to!S());
            assert();
            assert();
            assert();
            assert();
            assert();
        }}

        assert();
        assert();
        assert();
        assert();
        assert();

        assert();
        assert();
        assert();
        assert();
        assert();

        assert();
        assert();
        assert();
        assert();
        assert();

        assert();
        assert();
        assert();

        assert();
        assert();
    }}

    assert();
    assert();
    assert();
    assert();

    assert();
    assert();

    alias f = filter!"true";
    assert();
    assert();
    assert();
    assert();
    assert();
    assert();
    assert();
;
// https://issues.dlang.org/show_bug.cgi?id=10683
@safe     import std.range : join;
    import std.typecons : tuple;
    assert();
    assert();
// https://issues.dlang.org/show_bug.cgi?id=13877
@safe     // Test that the range is iterated only once.
    import std.algorithm.iteration : map;
    int c = 0;
    auto j1 = [1, 2, 3].map!().join;
    assert();
    assert();

    c = 0;
    auto j2 = [1, 2, 3].map!().join();
    assert();
    assert();

    c = ;
    auto j3 = [1, 2, 3].map!().join();
    assert();
    assert();
/++
    Replace occurrences of `from` with `to` in `subject` in a new array.

    Params:
        subject = the array to scan
        from = the item to replace
        to = the item to replace all instances of `from` with

    Returns:
        A new array without changing the contents of `subject`, or the original
        array if no match is found.

    See_Also:
        $(REF substitute, std,algorithm,iteration) for a lazy replace.
 +/
E"aaa.dd";


// #run: (font-lock-ensure)
