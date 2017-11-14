# `nix-deploy`
Deploy a Nix store path to another NixOS machine, optionally including any
additional NixOS modules

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
nix-deploy system --help
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
This is typically meant to be used in conjunction with the `nix-delegate` utility:

```shell
$ nix-delegate --host parnell@jenkins-slave-nixos01 --cores 4 --x86_64-linux --key /home/parnell/.ssh/awake nix-build --no-out-link -A awake-aaa ~/Development/work/awakenetworks/awake-pkgs/release.nix | nix-deploy path --to root@54.227.191.89
[+] Downloading: /etc/nix/signing-key.sec
[+] Installing: /etc/nix/signing-key.sec
[+] Downloading: /etc/nix/signing-key.pub
[+] Installing: /etc/nix/signing-key.pub
[+] Running command: sudo nix-build --no-out-link -A awake-aaa /home/parnell/Development/work/awakenetworks/awake-pkgs/release.nix
[+] Full command context: sudo NIX_BUILD_HOOK=/nix/store/jj3kq2dmllvkqwwbhnmzbk9hfgncdbvl-nix-1.11.6/libexec/nix/build-remote.pl NIX_PATH=ssh-config-file=/home/parnell/.ssh/config:ssh-auth-sock=/run/user/1000/ssh-agent:nixpkgs=/nix/store/qk1q2rwq9qzhi49hx7whji90bqk8kf9y-nixpkgs-7ae9da426924537755ce9164fd5b5f81ce16a1c3-src:nixpkgs=/nix/store/qk1q2rwq9qzhi49hx7whji90bqk8kf9y-nixpkgs-7ae9da426924537755ce9164fd5b5f81ce16a1c3-src:nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs:nixos-config=/etc/nixos/configuration.nix:/nix/var/nix/profiles/per-user/root/channels NIX_REMOTE_SYSTEMS=/tmp/remote-systems1957747793424238335.conf NIX_CURRENT_LOAD=/tmp/build-remote-load30593 nix-build --no-out-link -A awake-aaa /home/parnell/Development/work/awakenetworks/awake-pkgs/release.nix
these derivations will be built:
  /nix/store/kknibvnjwizqv5pnqhypy12qf6dr030g-awake-aaa-0.1.0.0.drv
copying 178 missing paths (474.70 MiB) to ‘parnell@jenkins-slave-nixos01’...
...
[+] Copying /nix/store/q4c3avwb0szbsg8pkv7x32gcqz4g0wwa-awake-aaa-0.1.0.0

copying 4 missing paths (31.83 MiB) to ‘root@54.227.191.89’...

```
