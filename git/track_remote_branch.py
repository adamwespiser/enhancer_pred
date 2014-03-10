#!/usr/bin/python

import argparse
import subprocess
import os, sys
import colorama
from colorama import Fore, Back, Style

class TrackRemoteBranch(object):
    def __init__(self):
        self.branches = []
        cmd = subprocess.Popen("git branch -a", shell=True, stdout=subprocess.PIPE)
        for line in cmd.stdout:
            self.branches.append(line.strip())

    def get_user_input(self):
        for idx, b in enumerate(self.branches):
            if 0 == idx and b.startswith("* "):
                print idx, Fore.RED + b
            else:
                print idx, b
        while True:
            answer = None
            try:
                s = raw_input( "Please make selection: " )
                answer = int( s )
            except KeyboardInterrupt, SystemExit:
                print ""
                sys.exit(0)
            except:
                print "could not parse \"" + s + "\""
                pass
            if answer >= 0 and answer <= len(self.branches):
                return answer

    def track_branch(self, idx):
        cmd = "git checkout --track " + self.branches[idx]
        os.system(cmd)

    def run(self):
        self.track_branch(self.get_user_input())


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--test', action="store_true", default=False)
    return parser.parse_args()

def main():
    colorama.init(autoreset=True)
    args = parse_args()

    trb = TrackRemoteBranch()
    trb.run()

if __name__ == "__main__":
    sys.exit(main())

