// -*- tab-width: 8 -*-
// #run: (progn (d-setup-cascaded-call-indentation) (d-test-indent))

import std.file;
import std.path;

void main()
{
  enum dirPath = "";
  foreach (file; dirPath.expandTilde()
			.buildNormalizedPath()
			.dirEntries(SpanMode.shallow))
    {}
  foreach (file; dirPath.expandTilde
			.buildNormalizedPath
			.dirEntries(SpanMode.shallow))
    {}
  foreach (file; dirPath.expandTilde
			.buildNormalizedPath!()()
			.dirEntries(SpanMode.shallow))
    {}
}
