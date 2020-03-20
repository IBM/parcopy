# parcopy
Recursive copy to and remove from an SMB share utilizing parallel connections

### Prerequisites
1. Linux
1. `mount.cifs` installed
1. [`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install) installed
1. `~/.local/bin/` in your `$PATH`

### Install
1. Clone this repo and `cd` into it
1. `stack setup` (required once before building)
1. `stack install` to build `parcp` and `parrm` and copy them to `~/.local/bin/`
1. These programs work via setuid:
```
for cmd in 'chown root:root' 'chmod a+s'; do
    sudo $cmd ~/.local/bin/{parcp,parrm}
done
```

### Usage
```
$ parcp --help
Recursive copy in parallel

Usage: parcp [-v|--verbose] [--debug] [-t|--threads INT]
             (-n|--share-name //HOST/SHARE) (-c|--credentials FILENAME) SOURCE
             DESTINATION

Available options:
  -h,--help                Show this help text
  -v,--verbose             Print every file operation
  --debug                  Print lots of debug info
  -t,--threads INT         How many parallel connections to
                           establish (default: 4)
  -n,--share-name //HOST/SHARE
                           The UNC name of the SMB share
  -c,--credentials FILENAME
                           The credentials file to pass to mount.cifs(8)
  SOURCE                   Directory to copy contents from
  DESTINATION              Directory to copy contents to
```
```
$ parrm --help
Recursive delete in parallel

Usage: parrm [-v|--verbose] [--debug] [-t|--threads INT]
             (-n|--share-name //HOST/SHARE) (-c|--credentials FILENAME) TARGET

Available options:
  -h,--help                Show this help text
  -v,--verbose             Print every file operation
  --debug                  Print lots of debug info
  -t,--threads INT         How many parallel connections to
                           establish (default: 4)
  -n,--share-name //HOST/SHARE
                           The UNC name of the SMB share
  -c,--credentials FILENAME
                           The credentials file to pass to mount.cifs(8)
  TARGET                   Directory to remove
```

### License
http://www.apache.org/licenses/LICENSE-2.0
