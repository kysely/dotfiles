local function set_dark_theme()
	vim.api.nvim_set_option_value("background", "dark", {})
	vim.cmd.colorscheme("catppuccin-mocha")
end

local function set_light_theme()
	vim.api.nvim_set_option_value("background", "light", {})
	vim.cmd.colorscheme("github_light")
end

local function macos_is_dark()
	if vim.fn.executable("defaults") == 0 then
		return true
	end

	local handle = io.popen("defaults read -g AppleInterfaceStyle 2>/dev/null")
	if not handle then
		return true
	end

	local result = handle:read("*a")
	handle:close()

	return result:match("Dark") ~= nil
end

local function set_system_theme()
	if macos_is_dark() then
		set_dark_theme()
	else
		set_light_theme()
	end
end

return {
	{
		"catppuccin/nvim",
		name = "catppuccin",
		lazy = false,
		priority = 1000,
	},
	{
		"projekt0n/github-nvim-theme",
		name = "github-theme",
		lazy = false,
		priority = 1000,
	},
	{
		"f-person/auto-dark-mode.nvim",
		lazy = false,
		priority = 999,
		dependencies = {
			"catppuccin/nvim",
			"projekt0n/github-nvim-theme",
		},
		main = "auto-dark-mode",
		opts = {
			fallback = "dark",
			update_interval = 3000,
			set_dark_mode = set_dark_theme,
			set_light_mode = set_light_theme,
		},
	},
	{
		"LazyVim/LazyVim",
		opts = {
			colorscheme = set_system_theme,
		},
	},
}
