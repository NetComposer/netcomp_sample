# IntelliJ Aspects

* Check in config in "Erlang External Tools" that is pointing to right rebar3
* Check in Project Structure that Project SDK is pointing to Erlang installations
* Mark src directories as source
* Mark include directories as include

## How to clone into a new project

* See [Clone Help](clone.md)
* Remove netcomp_sample.iml and .idea
* Create new project, "import from existing sources", use "create"
* Do not select 'use rebar', use raw sources
* Copy folder '.idea/runConfigurations' from netcomp_sample
* Check previous aspects

