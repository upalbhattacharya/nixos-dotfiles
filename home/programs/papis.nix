{ pkgs, ... }:

{
	programs.papis = {
		enable = true;
    libraries = {
      papers = {
       dir = "~/ReferenceManagement/papers";
      };
    };
	};
}
