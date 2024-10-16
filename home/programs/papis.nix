{ pkgs, ... }:

{
	programs.papis = {
		enable = true;
    libraries = {
      papers = "~/ReferenceManagement/papers";
    };
	};
}
