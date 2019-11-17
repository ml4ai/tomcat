Contributing
============

Suggested Git Workflow
----------------------

We subscribe to the philosophy of keeping the master branch deployable. This
means that pushing directly to the master branch is (in general) disallowed,
and all changes to it will be through pull requests. Additionally, the master
branch is protected - merging will not be possible until all the continuous
integration checks pass.

The following git workflow is suggested for contributors. To implement a new
feature, do the following:

- Assuming you are on the `master` branch, check out a new feature branch. If
  the name of the feature is `new_feature`, you would do `git checkout -b new_feature`.
- Implement the feature.
- Run `git status` to see what files have changed. Optionally, run `git diff` to
  see the details of the changes to refresh your memory (and catch errant print
  statements!).
- Add the new/changed files by doing 
  ```
  git add <filename1> <filename2> ... <filenameN>
  ```
  Remember:
  - Only add human-generated files. Examples include:
    - Source code: 
       - `.cpp`, `.hpp`, and `CMakeLists.txt` files
      - Documentation in the form of `.md` and `.rst` files.
  - **Do not run** `git add .` or `git add -A`! Instead, add the changed files
    individually - this will encourage you to make your commits less
    monolithic, which makes them easier to merge, and to revert if necessary.
    Also, this prevents the accidental addition of automatically-generated
    files to version control, which bloats the repository, and can even cause
    the automated test suite to fail.
- Commit the changes:
  ```
  git commit -m "Description of features/changes that were implemented."
  ```
- Push the changes:
  ```
  git push
  ```
- Wait for an email from [Travis CI](https://travis-ci.com/ml4ai/tomcat), to let
  you know whether the automated tests have passed or failed (if you haven't
  signed up for email alerts from Travis, you can simply view the live test
  execution log on the Travis website itself.) 
- If the branch build passes, 
  [open a pull request (PR)](https://help.github.com/articles/creating-a-pull-request/) 
  by going to the [repo website](https://github.com/ml4ai/tomcat).
- One of the repo maintainers will then review your PR, and merge it into the
  master branch.
- Once the feature branch is merged, do the following to update your master
  branch and delete your feature branch.
  ```
  git checkout master
  git pull
  git branch -D new_feature
  ```
- **Tip 1:**: In general, smaller pull requests are better, and easier to merge.
- **Tip 2:**: Whenever you get an email from Github telling you that a branch
    has been merged into the master branch, but you are in the middle of
    implementing your feature branch, make sure to pull the changes from master
    into your branch and resolve any merge conflicts (another reason to not
    delay PRs!). Assuming you are on the `new_feature` branch, you would do:
    ```
    git pull origin master --rebase
    ```
