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
						mode = "1920x1080@165.003Hz";
						position = "0,0";
					}
				];
			}
		];
	};
}
