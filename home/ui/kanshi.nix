{ config, pkgs, lib, ... }:

{
	services.kanshi = {
		enable = true;
		# systemdTarget = "";
		settings = [
			{
				profile.name = "solo";
				profile.outputs = [
					{
						criteria = "eDP-1";
						status = "enable";
						mode = "2560x1440@165.003Hz";
						position = "0,0";
					}
				];
			}
		];
	};
}
