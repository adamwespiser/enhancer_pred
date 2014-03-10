#!/usr/bin/env python

## also see https://help.github.com/articles/remove-sensitive-data

import argparse
import os, sys
import colorama
from colorama import Fore, Back, Style

sys.path.append(os.path.expanduser('~/Git.bitbucket/common/python'))
from get_yes_no import GetYesNoToQuestion
from git import Git

def parse_args():
    parser = argparse.ArgumentParser()
    #parser.add_argument('--test', action="store_true", default=False)
    parser.add_argument('filter', type=str, nargs='+')
    return parser.parse_args()

def main():
    colorama.init(autoreset=True)
    args = parse_args()

    if not Git.no_changes_exist():
        print Fore.GREEN + "please commit first"
        sys.exit(1)

    Git.cd_git_root()

    for f in args.filter:
        if os.system("git filter-branch -f --index-filter 'git rm --cached --ignore-unmatch {0}' --prune-empty --tag-name-filter cat -- --all".format(f)):
            raise Exception("invalid git filter-branch")

    if os.system("git reflog expire --expire=now --all"):
        raise Exception("invalid git reflog")

    Git.repack()

    if os.system("git gc --prune=now"):
        raise Exception("invalid git gc")

    print Fore.RED + "pushing to origin master will require recloning of all other cloned repositories"
    if not GetYesNoToQuestion.immediate(Fore.YELLOW + "push origin master?"):
        sys.exit(1)

    if os.system("git push origin master --force"):
        raise Exception("invalid git push")

if __name__ == "__main__":
    sys.exit(main())

