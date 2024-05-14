return {
	"wthollingsworth/pomodoro.nvim",
	cmd = { "PomodoroStart", "PomodoroStatus" },
	dependencies = { "MunifTanjim/nui.nvim" },
	opts = {
		time_work = 30,
		time_break_short = 5,
		time_break_long = 20,
		timers_to_long_break = 4,
	},
}
