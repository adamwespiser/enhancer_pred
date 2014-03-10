#!/usr/bin/python

import sys, os

sys.path.append(os.path.expanduser('~/Git.bitbucket/common/python'))
from git import Git

Git.repack()
