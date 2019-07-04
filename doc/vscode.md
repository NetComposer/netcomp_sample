# VSCode Aspects

* You can view this markdown with ⌘⇧P and then type `Markdown: Open Preview` (or `Markdown Open Preview to the Side` for editing)
* Install some extensions (at least, Erlang/OTP)
* Create a workspace file for a multi-root workspace (check [Multi-root workspaces]() for further info)
* If you can't find a text occurrence you should review your include/exclude definition (check [Search tips]() for further info)

## Useful keyboard shortcuts
OS X Keys: `Ctrl`, `⇧ Shift`, `⌘ Command`, `⌥ Alt`

* Hide/Show side bar: ⌘B (OS X)
* Open command palette: ⌘⇧P (OS X)
* List configured building tasks: ⌘⇧B (OS X)
* Search in currently opened file: ⌘F (OS X)
* Search globally: ⌘⇧F (OS X)
* Hide/Show integrated terminal: Ctrl< (OS X)
* Go to file: ⌘P (OS X)
* Split editor: Ctrl⌥⌘º (OS X)
* Increase font size: ⌘+ (OS X)
* Reduce font size: ⌘- (OS X)
* Reset font size: ⌘0 (Numpad 0) (OS X)
* Add extra cursor up/down to edit multiple lines at once: ⌥⌘↑, ⌥⌘↓ (OS X)
* Copy current line up/down: ⇧⌥↑, ⇧⌥↓ (OS X)
* Select next ocurrence of current word: ⌘D (OS X)
* Select all ocurrences of current word: ⌘⇧L (OS X)

## How to clone into a new project

* See [Clone Help](clone.md) (tip: use global search to rename application across multiple files)
* Remove netcomp_sample.iml and .idea (optional)
* Copy folder '.vscode' from netcomp_sample to be able to compile and reload your application easily
* If using multi-root workspaces, rename workspace file and contents accordingly

## Hot Code reloading tips
To be able to reload code without stopping your erlang application you can define a task that will execute a script that performs the code reloading.
Create a file named "tasks.json" inside .vscode folder like this:
```
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Reload modules",
            "type": "shell",
            "command": ".vscode/remote_reload.sh",
            "args": [],
            "group": "build",
            "presentation": {
                "echo": true,
                "reveal": "always",
                "focus": false,
                "panel": "shared"
            },
            "problemMatcher": []
        }
    ]
}
```
Then, create a file named "remote_reload.sh" like this:
```
#!/bin/bash
. $PWD/envs || . $PWD/../envs

echo "Compiling..."
make -C $CURRENT_DIR || exit 1

echo "Reloading modules..."

erl -pa $CURRENT_DIR \
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"nklib\")." \
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"nkpacket\")." \
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"nkrest\")." \
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"nkserver\")." \ # Include other dependencies if needed
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"${APP}\")." \
    -s init stop -noshell \
    -name reloader@127.0.0.1 \
    -setcookie nk \
    -pa $CURRENT_DIR/_build/default/lib/nklib/ebin \
    -pa $CURRENT_DIR/_build/default/lib/$APP/ebin

echo "DONE!"
```
Don't forget to give it execution rights with
```
chmod +x remote_reload.sh
```
You would also need a "envs" file at the root with at least these environment variables:
```
export APP=netcomp_sample
export CODE_LOADING_MODE=interactive
export CURRENT_DIR=$( cd $( dirname ${BASH_SOURCE[0]} ) && pwd )
export FQDN=localhost
#export FQDN=${HOSTNAME}.${APP}.nc.svc.cluster.local # alternative
export VSN=01
export HOSTNAME=local # optional
```

## Search tips
### Search modes
You can enable/disable three modes when performing a search:
* Uppercase/Lowercase: when enabled, results will match the exact word you've typped
* Complete words: when enabled, only complete words will be matched (searching "NAME" won't list "HOSTNAME")
* Regular expression: when enabled, you can type a regular expression in the search box
### Replacing
If you click on the white arrow next to the search box, then you will be able to type the replacement text. Once done, every result will show a preview of the replacement. You can apply said replacement globally, per file or per appearance
### Include/exclude files
To be able to limit your searches, you can define folders or extensions to be taken into account (or to be excluded from the results). A common include line will look like:
```
src/, include/, config/, _checkouts/, envs, Makefile, rebar.lock, rebar.config
```
While an exclude line will look like:
```
_build/
```
If you see unexpected results when searching, examine the includes and excludes to determine what needs to be changed
## Multi-root workspaces
A multi-root workspace is useful if you are working with several projects at the same time. To add a new folder to your current workspace, you can use `File > Add Folder to Workspace` command to bring up an Open Folder dialog. When finished, you can save your current workspace with `File > Save Workspace As...` command.

A different way to create workspace file is to directly type the folders in a text file named `my_project.code-workspace` such as:
```
{
    "folders": [
        {
            // "name": "DEPENDENCY 01",
            "path": "/path/to/project/_checkouts/dep_01"
        },
        {
            // "name": "DEPENDENCY 01",
            "path": "/path/to/project/_checkouts/dep_02"
        },
        ...
        {
            // "name": "DEPENDENCY N",
            "path": "/path/to/project/_checkouts/dep_N"
        },
        {
            // "name": "MAIN PROJECT",
            "path": "/path/to/project"
        }
    ],
    "settings": {}
}
```
Your file explorer will be organized by workspaces, following the same order defined in the workspace file. You can rearrange it in case you want to add a new dependency and keep the alphabetical order.

`Important!: There is a bug that will hide subrepositories from the GIT view if they are under an ignored folder (e.g. _checkouts). A workaround is to leave the main project at last position. This way, the previous GIT repositories will be loaded and will be able to be managed from VSCode`

You can check the following link for further info:

https://vscode-eastus.azurewebsites.net/docs/editor/multi-root-workspaces

## Send HTTP requests within VSCode

To do this you need the `REST Client` extension. Once installed, simply create a new file *.http (under /priv for example) like this:
```
// Variables defined
@base_url = http://localhost:9001
@content_type = application/json;charset=utf-8
@value_01 = Value01
@value_02 = Value02

###
GET {{base_url}}/path/to/ws HTTP/1.1

###
POST {{base_url}}/path/to/ws HTTP/1.1
Content-Type: {{content_type}}

{
  "field_01" : "{{value_01}}",
  "field_02" : "{{value_02}}"
}
```
Note: each request must be preceded by `###`

Once created, you can send the request by clicking `Send Request` under `###`
You can also export your HTTP request as a cURL by clicking right-button and then `Copy Request as cURL`
or export your request in one of the supported languages with `Generate Code Snippet`

More info at: https://marketplace.visualstudio.com/items?itemName=humao.rest-client

## Useful extensions
VSCode has tons of extensions that will help you to highlight syntax for a certain programming language, run browsers for easy web developping, make HTTP requests, manage git repositories, view OpenAPI YAML/JSON docs, etc. These are some of the ones I found interesting, feel free to add yours too:
* `Docker`: Adds syntax highlighting, commands, hover tips, and linting for Dockerfile and docker-compose files
* `Docker Linter`: Lint perl, python and/or ruby in your docker containers
* `erlang`: Adds support for the Erlang language to Visual Studio Code, including editing, building and debugging. If you experience problems with automatic indentation, set "erlang.autoIndent" to false
* `Erlang/OTP`: Erlang/OTP support with syntax highlighting, auto-indent and snippets (although it usually failes to apply indentation when finishing a clause with `end` keyword)
* `ESLint`: Integrates ESLint JavaScript into VS Code
* `Express`: Hosts current workspace with Express web server
* `GitHistory`: View git log, file history, compare branches or commits
* `Go`: Rich Go language support for Visual Studio Code
* `Material Theme`: The most epic theme now for Visual Studio Code
* `REST Client`: REST Client for Visual Studio Code
* `Spanish Language Pack for Visual Studio Code`: Language pack extension for Spanish
* `SVG Viewer`: SVG Viewer for Visual Studio Code
* `Swagger Viewer`: Swagger Viewer lets you preview and validate Swagger 2.0 and OpenAPI files as you type in Visual Studio Code
* `Task Master`: Task Master will automatically trawl your project directory for task scripts, e.g. npm, gulp, shell, python, ruby, powershell, perl scripts etc... and allow you to execute each task with the click of a mouse!
* `vscode-linter-erlc`: A linter for erlang, using the erlc compiler

## Theme customization and more

Checkout this link for further customizations:

https://dev.to/selrond/tips-to-use-vscode-more-efficiently-3h6p

I'd recommend you to follow the steps for:
* A settings primer
* Theme
* Indentation
* Minimap
* Whitespace (now you will notice a tabulated space easily)
* Smooth scrolling
* Caret improvements
* Final new line
* Trim trailing whitespace (yay!)
* Hide feedback smiley in the bottom bar (at last :D)
