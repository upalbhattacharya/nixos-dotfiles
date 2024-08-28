{ pkgs, ... }:

{
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "${pkgs.xautolock}/bin/xautolock -now";
    xautolock = {
      enable = true;
      detectSleep = true;
    };
  };
}
