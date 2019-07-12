CONTRIBUTING
============

Working with submodules
-----------------------

ToMCAT has a couple of submodule dependencies. See
https://git-scm.com/book/en/v2/Git-Tools-Submodules for an introduction to working
with submodules - some salient points from it are shown below.

When cloning the repository for the first time, you would need to add the
`--recursive` flag to get all the submodule dependencies.

```
git clone --recursive https://github.com/ml4ai/tomcat
```

You will also probably want to set the `git diff` and `git status` configs to
display information about the submodules.

```
git config --global diff.submodule log
git config status.submodulesummary 1
```

Speeding up builds with Gradle
------------------------------

You can speed up builds by installing Gradle and adding the line

```
org.gradle.daemon=true
```

to your `~/.gradle/gradle.properties` file. (Source:
https://stackoverflow.com/a/34738800)
