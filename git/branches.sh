
# check which branches contain a given commit:
git branch -r --contains <sha-of-commit>

# sorting branch list by last commit date:
git branch --sort=-committerdate

# show branches w/ last commit:
git branch -v

# show branches w/ last commit + remotes:
git branch -vv
