# How to clone into a new project

* Copy directory to 'netcomp_my'
* Remove _build, _checkouts, log, rebar.lock
* If using IntelliJ, remove .idea and netcomp_sample.iml
* Rename files in 'src' from 'netcomp_sample' to 'netcomp_my'
* Rename file in 'include/netcomp_sample.hrl' to 'netcomp_my.hrl'
* Change all references from 'netcomp_sample' to 'netcomp_my' in:
   * netcomp_my.app.src
   * netcomp_my_app.erl (define, supervisor call)
   * netcomp_my.hrl (in uppercase)
   * envs
* Do make
* Exclude from git ide-related files
* Distribute rebar.lock for tested versions

