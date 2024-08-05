{ config, pkgs, ... }:

{
	programs.zsh = {
  	enable = true;
  	syntaxHighlighting.enable = true;
  	autosuggestion.enable = true;
  	enableCompletion = true;
  	oh-my-zsh = {
    	enable = true;
    	plugins = [ 
      	"git"
      	"vi-mode"
    	];
  	theme = "robbyrussell";
  	};
	};
}
