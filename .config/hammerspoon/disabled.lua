-- Define the remapping from one key to another
local key_maps = {
	m = "return",
	i = "tab",
}
-- Define the set of modifiers that will be paired with 
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
		hs.hotkey.bind(
			trigger_mods, original_key,
			function() hs.eventtap.keyStroke(mods, target_key) end, --Executed once on press
			nil,		--Executed on release (nil to skip)
			function() hs.eventtap.keyStroke(mods, target_key) end) -- Executed while held down
	end
end
