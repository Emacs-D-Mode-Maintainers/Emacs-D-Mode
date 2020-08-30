// #run: (d-test-fontification)

module aaa;

import bbb;

module aaa.bbb.ccc;

import aaa.bbb;
import aaa.Bbb;
import aaa.bbb.ccc;
import Aaa.Bbb.ccc;
import Aaa.Bbb.Ccc;
import aaa.bbb.Ccc;

import ddd = aaa.bbb.ccc, vvv = xxx.yyy.zzz;
import aaa.bbb.ccc : x, y, z;
import aaa.bbb.ccc : i = x, j = y, k = z;

import a.b.c : d;
