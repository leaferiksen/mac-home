-- Define the remapping from one key to another
local key_maps = {
  m = "return",
  i = "tab",
}
-- Define the set of modifiers that will be paired with the 'ctrl' key
local optional_modifiers = {
  {},
  {"cmd"},
  {"alt"},
  {"shift"},
  {"cmd", "alt"},
  {"cmd", "shift"},
  {"alt", "shift"},
  {"cmd", "alt", "shift"},
}
-- Create the hotkey bindings dynamically
for original_key, target_key in pairs(key_maps) do
  for _, mods in ipairs(optional_modifiers) do
    -- The trigger hotkey always includes 'ctrl'
    local trigger_mods = hs.fnutils.copy(mods)
    table.insert(trigger_mods, "ctrl")
    -- The resulting keystroke uses only the original optional modifiers
    hs.hotkey.bind(trigger_mods, original_key, function()
      hs.eventtap.keyStroke(mods, target_key)
    end)
  end
end

hs.loadSpoon("SpoonInstall")
Install = spoon.SpoonInstall
Install.repos.Caffeine = {
	url = "https://github.com/Hammerspoon/Spoons/tree/master/Source/Caffeine.spoon",
	desc = "Caffeine.spoon repository",
	branch = "master",
}
Install:andUse("Caffeine", {
	start = true,
})
Install.repos.PaperWM = {
	url = "https://github.com/mogenson/PaperWM.spoon",
	desc = "PaperWM.spoon repository",
	branch = "release",
}
Install:andUse("PaperWM", {
	repo = "PaperWM",
	config = {
		window_gap = { top = 20, bottom = 20, left = 20, right = 20 },
		window_ratios = { 1/3, 1/2, 2/3, 1 }
	},
	fn = function(spoon)
		-- ignore a specific app
		spoon.window_filter:rejectApp("Finder")
		spoon.window_filter:rejectApp("Terminal")
		spoon.window_filter:rejectApp("QuickTime Player")
		spoon.window_filter:rejectApp("iPhone Mirroring")
		spoon.window_filter:rejectApp("Shortcuts")
		spoon.window_filter:rejectApp("Messages")
		spoon.window_filter:rejectApp("Facetime")
		spoon.window_filter:rejectApp("Stickies")
		spoon.window_filter:rejectApp("Minecraft")
		spoon.window_filter:rejectApp("IINA")
		spoon.window_filter:rejectApp("Elmedia Video Player")
		spoon.window_filter:rejectApp("Color Picker")
		spoon.window_filter:rejectApp("LuLu")
		spoon.window_filter:rejectApp("java")
		spoon.window_filter:rejectApp("Keynote")
		-- list of screens to tile (use % to escape string match characters, like -)
		spoon.window_filter:setScreens({ "Built%-in Retina Display" })
	end,
	start = true,
	hotkeys = {
		-- position and resize focused window
		cycle_width = {"alt", "c"},
		reverse_cycle_width = {"alt", "x"},
		-- move focused window into / out of a column
		slurp_in = {"alt", "i"},
		barf_out = {"alt", "o"},
		-- move the focused window into / out of the tiling layer
		toggle_floating = {"alt", "t"},
		focus_floating = {"alt", "s"},
		-- switch windows by cycling forward/backward
		-- (forward = down or right, backward = up or left)
		focus_prev = {"alt", "a"},
		focus_next = {"alt", "e"},
		-- Switch to a new focused window in tiled grid
		swap_left = {"alt", "b"},
		swap_right = {"alt", "f"},
		swap_up = {"alt", "p"},
		swap_down = {"alt", "n"},
	}
})
