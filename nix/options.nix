{ lib, ... }:
{ options = {
    deployment = {
      name = {
        type        = lib.types.str;
        description = ''
          This option specifies the name of the machine deployment.
        '';
      };

      machineAddress = {
        type        = lib.types.str;
        description = ''
          Address of the machine a system configuration is built for.
        '';
      };
    };
  };
}
