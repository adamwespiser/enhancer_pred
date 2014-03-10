#!/usr/bin/env python

# from http://stackoverflow.com/a/10099633

import os, sys

from multiprocessing import Pool

def getOutput(cmd):
    return os.popen(cmd).read()

def find_large_files(revision):
    bigfiles = set()
    files = getOutput("git ls-tree -zrl %s" % revision).split('\0')
    for file in files:
        if file == "":
            continue
        splitdata = file.split()
        commit = splitdata[2]
        if splitdata[3] == "-":
            continue
        size = int(splitdata[3])
        path = ' '.join(splitdata[4:])
        if (size > maxSize):
            bigfiles.add("%10d %s %s" % (size, commit, path))
    return bigfiles

def pretty_print(bigfiles):
    bigfiles = sorted(bigfiles, reverse=True)
    for f in bigfiles:
        print f

if __name__ == '__main__':
    if (len(sys.argv) <> 2):
        print "usage: %s size_in_bytes" % sys.argv[0]
        sys.exit(1)

    maxSize = int(sys.argv[1])
    revisions = getOutput("git rev-list HEAD").split()

    pool = Pool(processes=4)
    bf = pool.map(find_large_files, revisions)

    bigfiles = set()
    for files in bf:
        for f in files:
            bigfiles.add(f)

    pretty_print(bigfiles)
