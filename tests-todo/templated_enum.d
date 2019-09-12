/*
  The symbol here is mis-fontified as a function. The reason for this
  is that c-forward-decl-or-cast-1 understands the statement as not
  containing types, and then passes this information to
  c-font-lock-declarators. We can't get away with merely patching
  c-font-lock-declarators because at the moment it's called, it
  doesn't have enough information to know that it's actually dealing
  with an enum.
 */

enum bool isForwardRange(R) = 1;
