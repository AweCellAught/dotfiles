# BE ADVISED:
#
#   toscreen
#
#   tooscreen
#
#   twoscreen

import os
import subprocess
from libqtile import bar, layout, widget, hook, qtile
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen, Group, ScratchPad, DropDown, Rule
from libqtile.lazy import lazy

mod = "mod4"
alt = "mod1"
# terminal = "wezterm" 
terminal = "kitty" 

# MOVE TO SCREEN
def move_to_screen(screen):
    def inner(qtile):
        if len(qtile.screens) > screen:
            qtile.current_window.togroup(qtile.screens[screen].group.name)
    return inner

# @hook.subscribe.startup_once
# def autostart():
#     subprocess.Popen(['/home/jesse/daemon.sh', 'autostart-daemon'])
#     subprocess.Popen(['/home/jesse/daemon.sh', 'mpris-daemon'])
#     subprocess.Popen(['discord', '--no-startup-id', '--start-minimized'])
#     subprocess.Popen(['chromium', '--no-startup-window'])

@hook.subscribe.startup_once
def autostart():
    subprocess.Popen(["/home/jesse/daemon.sh"])

    subprocess.Popen(["sh", "-c", "sleep 1 && echo 'autostart-daemon' > /tmp/qtile_pipe"])

    subprocess.Popen(["sh", "-c", "sleep 1 && echo 'mpris-daemon' > /tmp/qtile_pipe"])

# WORKSPACE HISTORY
group_history = []  # Initialize the group_history variable

@hook.subscribe.setgroup
def update_history():
    update_group_history()

def update_group_history(*args, **kwargs):
    global group_history
    current_group = qtile.current_group.name

    if current_group in group_history:
        group_history.remove(current_group)

    group_history.append(current_group)
    group_history = group_history[-10:]

def toggle_group(qtile_instance):
    global group_history
    current_group = qtile_instance.current_group.name

    # find the most recent group that's not currently visible
    for group_name in reversed(group_history):
        if group_name != current_group:
            group = qtile_instance.groups_map.get(group_name)
            if group and not group.screen:
                group.toscreen()
                break

# TOPICS

# mapping of group names to actions:
group_actions = {
    '1': 'chromium',
    '2': 'discord',
    '3': 'chromium --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod',
    '4': 'chromium --profile-directory=Default --app-id=agimnkijcaahngcdmfeangaknmldooml',
    '5': '/home/jesse/daemon.sh priority-nvim',
    'F1': 'chromium --new-window https://www.twitch.tv/moderator',

}

def group_specific_action(qtile):
    current_group = qtile.current_group.name
    if current_group in group_actions:
        command = group_actions[current_group]
        subprocess.Popen(command.split())

# SCREEN SWAPPING
def twoscreen(qtile):
    current_screen = qtile.current_screen
    other_screen = qtile.screens[1] if current_screen.index == 0 else qtile.screens[0]
    
    # swapping workspaces:
    current_group = current_screen.group.name
    other_group = other_screen.group.name

    current_screen.set_group(other_screen.group)
    other_screen.set_group(qtile.groups_map[current_group])


keys = [

    # https://docs.qtile.org/en/latest/manual/config/lazy.html


    Key([], "Print", lazy.spawn("sh -c 'echo \"scrnt-everything\" > /tmp/qtile_pipe'"), desc="Screenshot everything"),
    Key(["control"], "Print", lazy.spawn("sh -c 'echo \"scrnt-cursor-select\" > /tmp/qtile_pipe'"), desc="Screenshot inside cursor selection"),
    KeyChord([mod], "Print", [
        Key([], "Scroll_Lock", lazy.spawn("sh -c 'echo \"scrnt-0\" > /tmp/qtile_pipe'"), desc="Screenshot screen 0"),
            Key([], "Pause", lazy.spawn("sh -c 'echo \"scrnt-1\" > /tmp/qtile_pipe'"), desc="Screenshot screen 1")
    ]),

    #    Key([], "Print", lazy.spawn("/home/jesse/daemon.sh scrnt-everything"), desc="Screenshot everything"),
    #    Key(["control"], "Print", lazy.spawn("/home/jesse/daemon.sh scrnt-cursor-select"), desc="Screenshot inside cursor selection"),
    #    KeyChord([mod], "Print", [
    #        Key([], "Scroll_Lock", lazy.spawn("/home/jesse/daemon.sh scrnt-0"), desc="Screenshot screen 0"),
    #        Key([], "Pause", lazy.spawn("/home/jesse/daemon.sh scrnt-1"), desc="Screenshot screen 1")
    #    ]),
    #

    Key([mod], "a", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "u", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "o", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "e", lazy.layout.up(), desc="Move focus up"),
    Key([mod, "shift"], "n", lazy.layout.previous(), desc="Move window focus to other window"),
    Key([mod], "n", lazy.layout.next(), desc="Move window focus to other window"),

    Key([mod, "shift"], "a", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "u", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "o", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "e", lazy.layout.shuffle_up(), desc="Move window up"),

    Key([mod, "control"], "a", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "u", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "o", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "e", lazy.layout.grow_up(), desc="Grow window up"),

    Key([mod, "shift"], "tab", lazy.layout.normalize(), desc="Normalize"),
    Key([mod], "s", lazy.layout.toggle_split(), desc="Toggle split"),

    # Key([mod], "Return", lazy.spawn("/home/jesse/daemon.sh trm"), desc="Launch terminal"),
    Key([mod], "Return", lazy.spawn("sh -c 'echo \"trm\" > /tmp/qtile_pipe'"), desc="Launch terminal"),


    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, "shift"], "minus", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),

    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
    Key([mod, "shift"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "shift"], "escape", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "j", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),

    # Move focus to screen 0
    Key([mod], "comma", lazy.to_screen(0)),
    # Move focus to screen 1
    Key([mod], "period", lazy.to_screen(1)),

    # Move window to screen 0
    Key([mod, "shift"], "comma", lazy.function(move_to_screen(0))),
    # Move window to screen 1
    Key([mod, "shift"], "period", lazy.function(move_to_screen(1))),

    Key([mod], "apostrophe", lazy.function(twoscreen)),
    Key([mod], "p", lazy.function(toggle_group)),
    Key([mod, alt], "space", lazy.function(group_specific_action)),

    # Key([mod], "KP_Enter", lazy.spawn("/home/jesse/daemon.sh mute")),
    # Key([mod], "KP_Up", lazy.spawn("/home/jesse/daemon.sh volume up")),
    # Key([mod], "KP_Down", lazy.spawn("/home/jesse/daemon.sh volume down")),

    # Key([mod], "KP_Begin", lazy.spawn("/home/jesse/daemon.sh play")),
    # Key([mod], "KP_Insert", lazy.spawn("/home/jesse/daemon.sh stop")),


    # Key([mod], "KP_Right", lazy.spawn("/home/jesse/daemon.sh next")),
    # Key([mod], "KP_Left", lazy.spawn("/home/jesse/daemon.sh prev")),

    # Key([mod], "KP_Delete", lazy.spawn("/home/jesse/daemon.sh mic-mute")),

    Key([mod], "KP_Enter", lazy.spawn("sh -c 'echo \"mute\" > /tmp/qtile_pipe'")),
    Key([mod], "KP_Up", lazy.spawn("sh -c 'echo \"volume up\" > /tmp/qtile_pipe'")),
    Key([mod], "KP_Down", lazy.spawn("sh -c 'echo \"volume down\" > /tmp/qtile_pipe'")),
    
    Key([mod], "KP_Begin", lazy.spawn("sh -c 'echo \"play\" > /tmp/qtile_pipe'")),
    Key([mod], "KP_Insert", lazy.spawn("sh -c 'echo \"stop\" > /tmp/qtile_pipe'")),
    
    Key([mod], "KP_Right", lazy.spawn("sh -c 'echo \"next\" > /tmp/qtile_pipe'")),
    Key([mod], "KP_Left", lazy.spawn("sh -c 'echo \"prev\" > /tmp/qtile_pipe'")),
    
    Key([mod], "KP_Delete", lazy.spawn("sh -c 'echo \"mic-mute\" > /tmp/qtile_pipe'")),


    Key([mod, alt], "KP_End", lazy.function(
            lambda qtile: [qtile.cmd_to_screen(0), qtile.groups_map["1"].cmd_toscreen(toggle=False), qtile.cmd_spawn("chromium")]
            )),

    Key([mod, alt], "KP_Down", lazy.function(
            lambda qtile: [qtile.cmd_to_screen(0), qtile.groups_map["2"].cmd_toscreen(toggle=False), qtile.cmd_spawn("discord")]
            )),
    Key([mod, alt], "KP_Page_Down", lazy.function(
            lambda qtile: [qtile.cmd_to_screen(1), qtile.groups_map["3"].cmd_toscreen(toggle=False), qtile.cmd_spawn("chromium --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod")]
            )),
    Key([mod, alt], "KP_Left", lazy.function(
            lambda qtile: [qtile.cmd_to_screen(0), qtile.groups_map["4"].cmd_toscreen(toggle=False), qtile.cmd_spawn("chromium --profile-directory=Default --app-id=agimnkijcaahngcdmfeangaknmldooml")]
            )),

]

### GROUPS ###

# Function to mimic XMonad's "greedy view":
def tooscreen(group_name):
    def f(qtile):
        group = qtile.groups_map[group_name]
        if group.screen and group.screen != qtile.current_screen:
            qtile.cmd_to_screen(group.screen.index)
        else:
            group.toscreen()
    return f

# Rename the first twelve groups to F1-F12:
groups = [Group("F" + str(i)) for i in range(1, 13)]

# Rename the latter twelve groups to 1-12:
groups.extend([Group(str(i - 12)) for i in range(13, 25)])

# Update key bindings for the first twelve groups:
for i, group in enumerate(groups[:12], start=1):
    keys.append(Key([mod], "F" + str(i), lazy.function(tooscreen(group.name)),
                     desc="Switch to group {}".format(group.name)))
    keys.append(Key([mod, alt], "F" + str(i), lazy.group[group.name].toscreen(),
                     desc="Greedy switch to group {}".format(group.name)))
    keys.append(Key([mod, "shift"], "F" + str(i), lazy.window.togroup(group.name),
                     desc="Move focused window to group {}".format(group.name)))

# Update key bindings for the latter twelve groups:
dvorak_top_row = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "bracketleft", "bracketright"]
for i, group in enumerate(groups[12:], start=13):
    key = dvorak_top_row[i - 13]
    keys.append(Key([mod], key, lazy.function(tooscreen(group.name)),
                     desc="Switch to group {}".format(group.name)))
    keys.append(Key([mod, alt], key, lazy.group[group.name].toscreen(),
                     desc="Greedy switch to group {}".format(group.name)))
    keys.append(Key([mod, "shift"], key, lazy.window.togroup(group.name),
                     desc="Move focused window to group {}".format(group.name)))

layouts = [

    layout.Bsp(border_focus='#ff0000', border_normal='#669999', border_on_single=False, border_width=3, fair=True, grow=8, lower_right=True, margin=4, margin_on_single=None, ratio=(1 + ((((1 + 2**0.5) / (1 + (1 + 2**0.5)))))), wrap_clients=True),
    layout.TreeTab(),

]

widget_defaults = dict(
    font="b612",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [ 
    Screen(
        top=bar.Bar(
            [
                widget.CurrentLayout(),
                widget.GroupBox(hide_unused=True),
                widget.Prompt(),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.StatusNotifier(),
                widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                widget.QuickExit(),
            ],
            30,
            background="200000",
            border_width=[0, 0, 2, 0],
            border_color=["000000", "000000", "4d4d4d", "000000"] 
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = [
Rule(Match(wm_class=['discord']), group="2"),
]  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = True
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        # *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ], fullscreen_border_width = 0, border_width = 0
)

auto_fullscreen = False
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
