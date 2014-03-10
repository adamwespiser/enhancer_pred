import subprocess
import os

class Git(object):
    @staticmethod
    def in_git_folder():
        return 0 == os.system('git rev-parse 2> /dev/null > /dev/null')

    @staticmethod
    def cd_git_root():
        root_dir = subprocess.check_output(["git rev-parse --show-cdup"], shell=True).strip()
        if root_dir:
            os.chdir(root_dir)

    @staticmethod
    def no_changes_exist():
        no_diff = 0 == os.system("git diff --exit-code > /dev/null")

        untracked_cmd = 'git status --porcelain 2>/dev/null| grep "^??" | wc -l'
        out = subprocess.check_output(untracked_cmd, shell=True).strip()
        no_untracked = 0 == int(out)

        return no_diff and no_untracked

    @staticmethod
    def repack():
        #http://gcc.gnu.org/ml/gcc/2007-12/msg00165.html
        if os.system("git repack -a -d --depth=250 --window=250 -f"):
            raise Exception("invalid git repack")
