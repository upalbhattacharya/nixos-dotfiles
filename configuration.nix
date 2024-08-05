{ config, lib, pkgs, ... }:


{
  imports = [
		./hardware-configuration.nix
	];

  # Allow Unfree
  nixpkgs.config.allowUnfree = true;

	# Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

	# Shell
	environment.shells = with pkgs; [ zsh ];
	users.defaultUserShell = pkgs.zsh;

	# Networking
  networking.hostName = "nixos-workboots";
	networking.networkmanager.enable = true;

  # Timezone
  time.timeZone = "Europe/Amsterdam";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
	i18n.defaultLocale = "en_US.UTF-8";

	# Graphical Environment
	
	# Display Manager
	services.xserver.displayManager.gdm.enable = true;
	services.displayManager.defaultSession = "sway";

	# Nvidia
	hardware.graphics = {
  	enable = true;
	};

	services.xserver.videoDrivers = [ "nvidia" ]; 

	hardware.nvidia = {
		package = config.boot.kernelPackages.nvidiaPackages.beta;
		modesetting.enable = true;
		powerManagement.enable = false;
		powerManagement.finegrained = false;
		open = false;
		nvidiaSettings = true;
		prime = {
			offload = {
				enable = true;
				enableOffloadCmd = true;
			};
			amdgpuBusId = "PCI:7:0:0";
			nvidiaBusId = "PCI:1:0:0";
		};
	};

	# Enable the X11 windowing system.
	services.xserver.enable = true;

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



	# Users

	users.users.workboots = {
  	isNormalUser = true;
  	home = "/home/workboots";
  	extraGroups = [ "wheel" "networkmanager" "power" "video" ];
  	useDefaultShell = true;
	};


	# Enable users
  nix.settings.allowed-users = [
	  "workboots"
  ];

	environment.systemPackages = with pkgs; [
		vim
		wget
		alacritty
		lshw
		git
		pulsemixer
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
	programs.zsh.enable = true;

	xdg = {
		portal = {
			enable = true;
			extraPortals = with pkgs; [
				xdg-desktop-portal-wlr
				xdg-desktop-portal-gtk
			];
		};
	};    

	programs.sway = {
  	enable = true;
  	wrapperFeatures.gtk = true;
  	extraOptions = [
    	"--unsupported-gpu"
  	];
	};
}
