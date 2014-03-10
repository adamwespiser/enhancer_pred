#!/usr/bin/python

import argparse
import sys
import os
import colorama
from colorama import Fore, Back, Style

sys.path.append(os.path.expanduser('~/Git.bitbucket/common/python'))
from get_yes_no import GetYesNoToQuestion
from git import Git

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--test', action="store_true", default=False)
    return parser.parse_args()

def main():
    colorama.init(autoreset=True)
    args = parse_args()

    if not Git.in_git_folder():
        print Fore.RED + "not in git folder"
        sys.exit(1)
    Git.cd_git_root()
    if Git.no_changes_exist():
        print Fore.GREEN + "nothing to commit"
        #if Git.unpushed_commits():
        sys.exit(0)
    if os.system("git status"):
        raise Exception("invalid git status")

    if not GetYesNoToQuestion.immediate(Fore.YELLOW + "commit?"):
        sys.exit(0)

    if os.system("git add -A"):
        raise Exception("invalid git add")

    if os.system("git commit"):
        raise Exception("invalid git commit")
    print Fore.YELLOW + "pushing..."
    if os.system("git push"):
        raise Exception("invalid git push")

if __name__ == "__main__":
    sys.exit(main())

