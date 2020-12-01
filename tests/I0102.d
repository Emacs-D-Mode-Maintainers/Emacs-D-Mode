// #min-version: 26.1
// #run: (d-test-indent)

version (x86) {
  // Decl
} else version (x86_64) {
  // Decls
} else version (PPC64) {
  // More decls
} else
    static assert(0);
