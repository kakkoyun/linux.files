NOTE: THIS CODE IS UNMAINTAINED, but is believed to still work fine.

--------------------------------------------------
ledmon is designed to be used with xmonad / xmobar or any environment
that needs updates on the state of the caps lock, num lock, and scroll
lock keys.

ledmon will output a line of data to stdout on invocation, and another
line whenever the status of these toggles changes.

If caps lock is on, the line will contain the word CAPS.  Likewise
with NUM and SCROLL.  If all three are off, the line will be empty.
