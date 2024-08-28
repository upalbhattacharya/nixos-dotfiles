{ ... }:

{
  programs.nixvim = {
    globals.mapleader = ",";
    globals.maplocalleader = "//";
    keymaps = [
    {
      key = "<Esc>";
      mode = "n";
      action = "<cmd>nohlsearch<CR>";
    }
    
    # Telescope
    {
      key = "<leader>ff";
      mode = "n";
      action = "<cmd>Telescope find_files<CR>";
    }

    {
      key = "<leader>fg";
      mode = "n";
      action = "<cmd>Telescope live_grep<CR>";
    }

    {
      key = "<leader>fb";
      mode = "n";
      action = "<cmd>Telescope buffers<CR>";
    }

    {
      key = "<leader>ft";
      mode = "n";
      action = "<cmd>Telescope treesitter<CR>";
    }

    {
      key = "<leader>fm";
      mode = "n";
      action = "<cmd>Telescope marks<CR>";
    }

    {
      key = "<leader>fr";
      mode = "n";
      action = "<cmd>Telescope registers<CR>";
    }

    {
      key = "<leader>fl";
      mode = "n";
      action = "<cmd>Telescope lsp_definitions<CR>";
    }
    
    # conform.nvim
    {
      key = "<localleader>f";
      mode = "n";
      action = "<cmd>lua require('conform').format()<CR>";
    }

    # UndoTree
    {
      key = "<leader>fu";
      mode = "n";
      action = "<cmd>UndoTreeToggle<CR>";
    }

    # NvimTree
    {
      key = "<leader>e";
      mode = "n";
      action = "<cmd>NvimTreeToggle<CR>";
    }

    # Trouble
    {
      key = "<leader>xx";
      mode = "n";
      action = "<cmd>Trouble diagnostics toggle<CR>";
    }

    # Buffer navigation
    {
      key = "gb";
      mode = "n";
      action = "<cmd>BufferNext<CR>";
    }

    {
      key = "gB";
      mode = "n";
      action = "<cmd>BufferPrevious<CR>";
    }

    ];
  };
}
