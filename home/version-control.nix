{ pkgs, ... }:

{

	programs.git = {
		enable = true;
		package = pkgs.gitAndTools.gitFull; # Install git wiith all the optional extras
		userName = "upalbhattacharya";
		userEmail = "upal.bhattacharya@gmail.com";
		extraConfig = {
			# Use vim as our default git editor
			core.editor = "vim";
			# Cache git credentials for 15 minutes
			credential.helper = "cache";
		};
	};
}
