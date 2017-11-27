# `nix-deploy`
Deploy a NixOS system configuration with `nix-deploy system ...` to a remote
machine and switch the machine to that system configuration. You can also deploy
a nix store path with `nix-deploy path ...` to a remote machine or from a remote
machine.

This tool is often used in conjunction with [`nix-delegate`](https://github.com/awakesecurity/nix-delegate).

```shell
$ nix-deploy --help
Deploy software or an entire NixOS system configuration to another NixOS system

Usage: nix-deploy (path | system)

Available options:
  -h,--help                Show this help text

Available commands:
  path
  system
```

```shell
$ nix-deploy path --help
Usage: nix-deploy path (--to USER@HOST | --from USER@HOST) [--sudo] [--noSign]
                       [--path FILEPATH] [--profilePath FILEPATH]
                       [--profileName LINE]

Available options:
  -h,--help                Show this help text
  --to USER@HOST           Deploy software to this address (ex:
                           user@192.168.0.1)
  --from USER@HOST         Deploy software from this address (ex:
                           user@192.168.0.1)
  --sudo                   Prepend with sudo
  --noSign                 Don't sign payload (not recommended)
  --path FILEPATH          Path to deploy
  --profilePath FILEPATH   Path to parent profile directory (default:
                           /nix/var/nix/profiles)
  --profileName LINE       Name of profile to set (example: upgrade-tools)
```

```shell
$ nix-deploy system --help
Usage: nix-deploy system (--to USER@HOST | --from USER@HOST) [--noSign]
                         [--path FILEPATH] [--systemName LINE] ([--switch] |
                         [--boot] | [--test] | [--dry-activate] | [--reboot])

Available options:
  -h,--help                Show this help text
  --to USER@HOST           Deploy software to this address (ex:
                           user@192.168.0.1)
  --from USER@HOST         Deploy software from this address (ex:
                           user@192.168.0.1)
  --noSign                 Don't sign payload (not recommended)
  --path FILEPATH          Path to deploy
  --systemName LINE        Alternative system profile name (default: system)
```

## Usage example
```shell
$ nix-deploy --to parnell@remote-server --path $(nix-build --no-out-link --attr foo ~/Development/bar/release.nix)
[+] Downloading: /etc/nix/signing-key.sec
[+] Installing: /etc/nix/signing-key.sec
copying 178 missing paths (474.70 MiB) to ‘parnell@remote-server’...
...
[+] Copying /nix/store/q4c3avwb0szbsg8pkv7x32gcqz4g0wwa-foo-0.1.0.0

copying 4 missing paths (31.83 MiB) to ‘parnell@remote-server’...

```
