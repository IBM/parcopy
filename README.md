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
1. `stack install` to build `parcp` and `parrm` and copy them to `./local/bin/`
1. These programs work via setuid:
```
for cmd in 'chown root:root' 'chmod a+s'; do
    sudo $cmd ~/.local/bin/{parcp,parrm}
done
```

### Usage
`parcp --help`  
`parrm --help`

### License
http://www.apache.org/licenses/LICENSE-2.0
