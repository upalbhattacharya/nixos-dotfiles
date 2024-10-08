{ config, pkgs, lib, ... }:

{
	services.kanshi = {
		enable = true;
    systemdTarget = "";
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
      {
				profile.name = "home";
				profile.outputs = [
					{
						criteria = "eDP-1";
						status = "disable";
					}
          {
            criteria = "LG Electronics LG ULTRAGEAR+ 406NTYT11186";
            status = "enable";
            mode = "3840x2160@144.05Hz";
            position = "0,0";
          }
				];
			}
		];
	};
}
