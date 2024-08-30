{ pkgs, ... }:

{
  imports = [
    ./formatters.nix
    ./linters.nix
    # ./extraPlugins.nix
    ./keybindings.nix
  ];
	programs.nixvim = {
		enable = true;
		opts = {
			encoding = "utf-8";
			fileencoding = "utf-8";
			syntax = "enable";
			autoread = true;
			## vim.api.nvim_command("filetype plugin indent on");
			backup = false;
			writebackup = false;
			swapfile = false;
			hidden = true;
			fileformat = "unix";
			completeopt = "preview,menu,menuone,noselect";
			textwidth = 80;
			formatoptions = "tcqj";
			number = true;
			relativenumber = true;
			cursorline = true;
			cursorcolumn = true;
			colorcolumn = "100";
			splitright = true;
			splitbelow = true;
			termguicolors = true;
			# norecase = true;
			smartcase = true;
			timeout = true;
			timeoutlen = 300;
			updatetime = 100;
			clipboard = "unnamedplus";
			tabstop = 2;
			softtabstop = 2;
			shiftwidth = 2;
			expandtab = true;
			smarttab = true;
			autoindent = true;
			showmatch = true;
			hlsearch = true;
			smartindent = true;
			undolevels = 1000;
			undofile = true;
      spell = true;
			# undodir = vim.fn.expand("~/.vim/undodir");
			# loaded_netrw = 1;
			# loaded_netrwPlugin = 1;
		};
		plugins = {
			lualine = {
        enable = true;
        sectionSeparators = {
          right = "";
          left = "";
        };
        componentSeparators = {
          right = "";
          left = "";
        };
      };
      coq-nvim = {
        enable = true;
        settings = {
          auto_start = true;
          completion.always = true;
        };
      };
			lsp = {
				enable = true;
				servers = {
					nil-ls.enable = true;
					lua-ls.enable = true;
					pylsp.enable = true;
					texlab.enable = true;
					tsserver.enable = true;
				};
			};
			conform-nvim = {
				enable = true;
        settings = {
          formatters_by_ft = {
            lua = [ "stylua" ];
            python = [ "isort" "black" ];
            javascript = [ "prettier" ];
            typescript = [ "prettier" ];
            markdown = [ "prettier" ];
            yaml = [ "prettier" ];
            tex = [ "latexindent" ];
            sh = [ "shellcheck" ];
				  };
				  format_on_save = {
            lspFallback = true;
            timeoutMs = 500;
				  };
        };
			};
			lint = {
				enable = true;
				lintersByFt = {
					python = [ "pflake8" ];
					sh = [ "shellcheck" ];
					nix = [ "statix" ];
				};
			};
			trouble = {
				enable = true;
				settings = {
					mode = "document_diagnostics";
				};
			};
			telescope = {
				enable = true;
				extensions = {
					file-browser.enable = true;

				};
			};
			barbar = {
				enable = true;
			};
			indent-blankline = {
				enable = true;
			};
			hardtime = {
				enable = true;
			};
			which-key = {
				enable = true;
			};
			leap = {
				enable = true;
			};
			undotree = {
				enable = true;
			};
			gitsigns = {
				enable = true;
			};
			fugitive = {
				enable = true;
			};
			comment = {
				enable = true;
			};
			spider = {
				enable = true;
				skipInsignificantPunctuation = false;
			};
      nvim-tree = {
        enable = true;
      };
      mini = {
        enable = true;
        modules = {
          surround = { };
        };
      };
      nvim-colorizer = {
        enable = true;
      };
		};
		extraPlugins =  [
			pkgs.vimPlugins.catppuccin-nvim
			pkgs.vimPlugins.nvim-treesitter.withAllGrammars
		];
		colorscheme = "catppuccin";
	};
}
