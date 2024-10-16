{ pkgs, ... }:

{
	programs.papis = {
		enable = true;
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
