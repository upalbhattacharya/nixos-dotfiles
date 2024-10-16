{ pkgs, ... }:

{
	programs.papis = {
		enable = true;
    settings = {
      editor = "emc";
      opentool = "okular";
    };
    libraries = {
      papers = {
        name = "papers";
        settings = {
          dir = "~/ReferenceManagement/papers";
        };
        isDefault = true;
      };
    };
	};
}
