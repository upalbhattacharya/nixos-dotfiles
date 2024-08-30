{ config, lib, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    catppuccin.enable = true;
    plugins = [
    pkgs.tmuxPlugins.continuum
    pkgs.tmuxPlugins.resurrect
    pkgs.tmuxPlugins.sensible
    pkgs.tmuxPlugins.yank
    ];
    extraConfig = ''
    set-option -g status on
    set-option -g status-interval 5
    set-option -g status-keys vi
    set-option -g status-position top
    set-option -g status-right ""
    set-option -g status-left " #{session_name} #[bg="#1E2030",fg="#EE99A0"]"
    set-option -g status-left-style bg="#EE99A0",fg="#1E2030"
    set-option -g status-bg "#1E2030"
    set-option -g status-fg "#CAD3F5"

    set-window-option -g window-status-separator ""

    set-window-option -g window-status-format " #I: #W #[bg="#1E2030",fg="#363A4F"]"
    set-window-option -g window-status-style bg="#363A4F",fg="#1E2030"

    set-window-option -g window-status-current-format " #I: #W #[bg="#1E2030",fg="#A6DA95"]"
    set-window-option -g window-status-current-style bg="#A6DA95",fg="#1E2030"

    %if #{TMUX}
    set -g status-bg red
    %endif

    set -g default-terminal "tmux-256color"
    set -ag terminal-overrides ",xterm-256color:RGB"

    set -g bell-action none

    set -g remain-on-exit off

    set -g mouse on
    unbind -n MouseDrag1Pane
    unbind -Tcopy-mode MouseDrag1Pane

    bind m set monitor-activity
    bind y set synchronize-panes\; display 'synchronize-panes #{?synchronize-panes,on,off}'

    set -g set-titles on

    set -g set-titles-string '#S:#I.#P #W'

    setw -g automatic-rename

    set-window-option -g mode-keys vi
    bind h select-pane -L
    bind j select-pane -D
    bind k select-pane -U
    bind l select-pane -R
    bind-key -T copy-mode-vi v send -X begin-selection
    bind-key -T copy-mode-vi V send -X select-line
    bind-key -T copy-mode-vi y send -X copy-pipe-and-cancel 'xclip -in -selection clipboard'
    '';
  };
}
