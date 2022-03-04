Steps for deploying a component to the TA3 testbed
==================================================

- Create a PR into the master/main branch with the necessary changes. Also, if
  there is a file like `VERSION` or `version.sbt` that contains the version
  number, bump the version number in it appropriately.
    - Bugfixes will increment the patch version number (e.g. 3.0.1 to 3.0.2).
    - New backwards-compatible features and improvements will update the minor
      version number (e.g. 3.0.1 to 3.1.0). Here, backwards-compatible means
      that the format of the JSON messages either does not change or changes in
      a way that code that could process the previous format can also process
      the new format. For example, adding a key in the `data` part of the
      message would be backwards-compatible, while removing an existing key
      would not.
    - Backwards-incompatible changes should bump the major version number (e.g.
      3.0.1 --> 4.0.0)
- Once the CI tests pass, merge the PR into the main branch.
- Create a release on Github.
- Switch to the main/master branch, run `git pull` to make sure you have the
  latest code, then run `./scripts/deploy_to_gitlab`.
- Go into the TA3 testbed repo
- Switch to the `develop` branch
- Run `git pull` to make sure you have the latest code from the `develop`
  branch.
- Make a new branch off of `develop`
- Update the agent version number in the `docker-compose.yml` file (or multiple
  file(s) in your agent directory under the `Agents` directory. The agent
  version number should match the version you just deployed to the Gitlab
  container registry.
- Update the `releaseNotes.md` (located in the top-level `testbed` directory)
  file with a summary of the changes.
- Push your changes to the testbed upstream repository
- Check the changes to the code and make sure your edits are there.
- Go to the testbed repo webpage in your browser, and then create a merge
  request to merge your branch into the `develop` branch.
- Include a link to the GitHub release you created in the testbed merge
  request.
- Once the your merge request is accepted, delete the branch in the testbed
  repo.
