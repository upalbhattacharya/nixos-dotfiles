{
  config,
  lib,
  pkgs,
  inputs,
  system,
  ...
}:
let
  lowBatteryNotifier = pkgs.writeScript "lowBatteryNotifier" ''
    BAT_PCT=`cat /sys/class/power_supply/BAT0/capacity`
    BAT_STA=`cat /sys/class/power_supply/BAT0/status`
    echo "`date` battery status:$BAT_STA percentage:$BAT_PCT"
    test $BAT_PCT -lt 15 && test $BAT_PCT -gt 5 && test $BAT_STA = "Discharging" && DISPLAY=:0.0 DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus" notify-send -u critical "Low Battery" "Would be wise to keep my charger nearby."
    test $BAT_PCT -lt 5 && test $BAT_STA = "Discharging" && DISPLAY=:0.0 DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus" notify-send -u critical "Low Battery" "Charge me or watch me die!"
  '';
in
{
  imports = [ ./hardware-configuration.nix ];

  # Allow Unfree
  nixpkgs.config.allowUnfree = true;

  # Enable flakes
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 10;
  boot.loader.efi.canTouchEfiVariables = true;

  # Shell
  environment.shells = with pkgs; [ zsh ];
  users.defaultUserShell = pkgs.zsh;

  # Networking
  networking.hostName = "nixos-workboots";
  networking.networkmanager.enable = true;
  networking.enableIPv6 = false;

  # Timezone
  time.timeZone = "Europe/Amsterdam";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  security.pam.services.hyprlock = { };

  # Networking
  networking.firewall.allowedTCPPorts = [
    8384
    22000
  ];
  networking.firewall.allowedUDPPorts = [
    22000
    21027
  ];

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Graphical Environment

  # Display Manager
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.displayManager.gdm.autoSuspend = false;

  services.greetd.enable = true;
  services.greetd.settings = {
    default_session = {
      command = "${pkgs.greetd.tuigreet}/bin/tuigreet --remember --remember-session --cmd hyprland";
    };
  };

  # Nvidia
  hardware.graphics = {
    enable = true;
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.beta;
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
    prime = {
      # offload = {
      #   enable = true;
      #   enableOffloadCmd = true;
      # };
      sync.enable = true;
      amdgpuBusId = "PCI:7:0:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  # Configure keymap in X11
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Audio
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };
  services.blueman.enable = true;

  # TLP
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 20;

      #Optional helps save long term battery health
      START_CHARGE_THRESH_BAT0 = 40; # 40 and bellow it starts to charge
      STOP_CHARGE_THRESH_BAT0 = 80; # 80 and above it stops charging

    };
  };

  # Cron
  services.cron = {
    enable = true;
    systemCronJobs =
      let
        username = "workboots";
      in
      [
        "* * * * * ${username}  sh -x ${lowBatteryNotifier} > /tmp/cron.batt.log 2>&1"
      ];
  };

  # Lid Behaviour
  services.logind = {
    lidSwitchExternalPower = "ignore";
    lidSwitch = "ignore";
  };

  # Users

  users.users.workboots = {
    isNormalUser = true;
    home = "/home/workboots";
    extraGroups = [
      "wheel"
      "networkmanager"
      "power"
      "video"
      "uinput"
    ];
    useDefaultShell = true;
    packages = [

      # Utilities
      pkgs.tree
      pkgs.wl-clipboard-rs
      pkgs.ripgrep
      pkgs.jq
      pkgs.socat
      pkgs.graphviz
      pkgs.poetry
      pkgs.python312Full
      pkgs.pandoc
      pkgs.texliveFull
      pkgs.cmake
      pkgs.libvterm
      pkgs.libtool
      pkgs.ispell
      pkgs.fzf
      pkgs.zip
      pkgs.unzip
      pkgs.stow
      pkgs.rclone

      # Services
      pkgs.wpaperd
      pkgs.hypridle
      pkgs.mako
      pkgs.syncthing

      # UI
      pkgs.kanshi
      pkgs.fuzzel
      pkgs.hyprlock
      pkgs.hyprland
      pkgs.waybar
      pkgs.brightnessctl

      # Programs
      pkgs.discord
      pkgs.emacs30-pgtk
      pkgs.frescobaldi
      pkgs.lilypond
      pkgs.plantuml
      pkgs.zotero-beta
      pkgs.anki-bin
      pkgs.localsend
      pkgs.sioyek
      pkgs.mpv
      pkgs.kitty
      pkgs.vivaldi

      pkgs.protonvpn-cli_2
      pkgs.openvpn

      # Shell
      pkgs.zsh
      pkgs.starship
    ];
  };

  # Enable users
  nix.settings.allowed-users = [ "workboots" ];

  environment.systemPackages = with pkgs; [
    vim
    wget
    htop
    kitty
    lshw
    git
    libnotify
    gcc
    greetd.tuigreet
    pulsemixer
    networkmanagerapplet
    recutils
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  system.stateVersion = "24.05"; # Did you read the comment?

  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
  };

  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };
    mime = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "sioyek.desktop" ];
      };
    };
  };

  fonts = {
    packages = with pkgs; [
      nerd-fonts.inconsolata
      nerd-fonts.iosevka
    ];
    fontconfig = {
      antialias = true;
      hinting = {
        enable = true;
        style = "full";
        autohint = true;
      };
    };
  };

  # programs.sway = {
  #   enable = true;
  #   wrapperFeatures.gtk = true;
  #   extraOptions = [
  #     "--unsupported-gpu"
  #   ];
  # };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.hyprland = {
    enable = true;
    portalPackage = pkgs.xdg-desktop-portal-wlr;
  };

  programs.dconf.enable = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };
  programs.npm = {
    enable = true;
  };
}
