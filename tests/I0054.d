// #run: (d-test-fontification)

module aaa;

import bbb;

// FIXME:

module aaa.bbb.ccc;

import aaa.bbb;
import aaa.Bbb;
import aaa.bbb.ccc;
import Aaa.Bbb.ccc;
import Aaa.Bbb.Ccc;
import aaa.bbb.Ccc;
