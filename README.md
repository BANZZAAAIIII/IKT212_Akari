# IKT212_Akari
How to setup for bitbucket

1. Add the remote repository link
  - git remote add bitbucket [repo-link](https://tools.uia.no/bitbucket/projects/IKT212G21H/)
2. Bitbucket uses main as defualt branch, copy master over
  - git branch main master
3. Push main branch to bitbucket, assumes you have added credentials
  - git push bitbucket main
    - git push -u bitbucket main

As long as you don't make changes to any files in main, there should never be any conflicts merging master to main before pushing to bitbucket.
