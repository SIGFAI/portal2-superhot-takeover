::HotCap <- function(txt, sec) { ::HotSay("cap", txt, sec) }
// Demo: constant strafing so time pulses, an early kill, portal trick, cube crush, rams.
::HotStrafe <- { left = true }
SigfDemo(0.5, function() {
	SigfLook(8, ::HT.homeYaw)
	HotCap("SUPERHOT x PORTAL 2", 3)
})
SigfDemo(1, function() {
	SigfEvery(3.0, function() {
		if (::HT.charge != null) return
		SigfHold(::HotStrafe.left ? "moveleft" : "moveright", 1.5)
		::HotStrafe.left = !::HotStrafe.left
	})
})
SigfDemo(3.5, function() {
	HotCap("RAM A TURRET: SHATTER", 4)
	::HotRam()
})
SigfDemo(10, function() {
	::HotGoHome()
	::HotSay("note", "Stand still and time crawls. Move and it runs.", 5)
	HotCap("TIME ONLY MOVES WHEN YOU MOVE", 5)
})
SigfDemo(16, function() {
	::HotGoHome()
	HotCap("BULLETS FLY THROUGH PORTALS", 5)
	::HotPortalTrick()
})
SigfDemo(28, function() {
	::HotGoHome()
	HotCap("COMPANION CUBE DROP: CRUSH", 5)
	::HotCubeDrop()
})
SigfDemo(40, function() {
	::HotGoHome()
	HotCap("RAM AGAIN", 4)
	::HotRam()
})
SigfDemo(50, function() {
	::HotGoHome()
	HotCap("SUPER. HOT.", 4)
	::HotRam()
})
SigfDemo(60, function() {
	::HotGoHome()
	HotCap("SUPERHOT TAKEOVER", 6)
	::HotRam()
})
