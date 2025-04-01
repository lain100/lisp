#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
#SingleInstance force
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

vk1C & LShift::return
LShift & h::Send, {Blind}{Numpadmult}
LShift & l::Send, {Blind}{Numpaddiv}
LShift & j::Send, {Blind}{Numpadadd}
LShift & k::Send, {Blind}{Numpadsub}
LShift & u::Send, {Blind}{(}
LShift & i::Send, {Blind}{)}
vk1C & k::Send, {Blind}{Up Down}
vk1C & k Up::Send, {Blind}{Up Up}
vk1C & j::Send, {Blind}{Down Down}
vk1C & j Up::Send, {Blind}{Down Up}
vk1C & h::
  if GetKeyState( "LShift", "P" )
    Send, {Blind}{Home}
  else
    Send, {Blind}{Left Down}
  return
vk1C & h Up::Send, {Blind}{Left Up}
vk1C & l::
  if GetKeyState( "LShift", "P" )
    Send, {Blind}{End}
  else
    Send, {Blind}{Right Down}
  return
vk1C & l Up::Send, {Blind}{Right Up}
vk1C & o::Send, {Blind}{BS}
vk1C & p::Send, {Blind}{Del}
vk1C & e::Send, {Blind}{Enter}
vk1C & 1::Send, {Blind}{!}
vk1C & 2::Send, {Blind}{@}
vk1C & 3::Send, {Blind}{#}
vk1C & 4::Send, {Blind}{$}
vk1C & 5::Send, {Blind}{`%}
vk1C & 6::Send, {Blind}{^}
vk1C & 7::Send, {Blind}{&}
vk1C & u::Send, {Blind}{(}
vk1C & i::Send, {Blind}{)}
vk1C & 8::Send, {Blind}{~}
vk1C & 9::Send, {Blind}{|}
vk1C & -::Send, {Blind}{=}
*@::Send, {Blind}{[}
vk1C & @::Send, {Blind}{`{}
*[::Send, {Blind}{]}
vk1C & [::Send, {Blind}{}}
vk1C & vkBB::Send, {Blind}{:}
*vkBA::Send, {Blind}{'}
vk1C & vkBA::Send, {Blind}{"}
*]::Send, {Blind}{``}
vk1C & ,::Send, {Blind}{<}
vk1C & .::Send, {Blind}{>}
vk1C & /::Send, {Blind}{?}
vk1C & vkE2::Send, {Blind}{_}
^1::Send, ^{Blind}{F1}
^2::Send, ^{Blind}{F2}
^3::Send, ^{Blind}{F3}
^4::Send, ^{Blind}{F4}
^5::Send, ^{Blind}{F5}
^6::Send, ^{Blind}{F6}
^7::Send, ^{Blind}{F7}
^8::Send, ^{Blind}{F8}
^9::Send, ^{Blind}{F9}
^0::Send, ^{Blind}{F10}
^-::Send, ^{Blind}{F11}
^^::Send, ^{Blind}{F12}
^j::Send, {Blind}^{PgUp}
^l::Send, {Blind}^{PgDn}
; change vkF0 to Ctrl in regedit
+vk14::Return
vk90::Return
*Esc::Send, {Blind}{vkF2}{vkF3}{Esc}
*RAlt::Send, {Blind}{vk5D}
*Space::Send, {Blind}{Shift Down}
*Space Up::
  Send, {Blind}{Shift Up}
  if (A_PriorKey == "Space")
    Send, {Blind}{Space}
  return
vk1D & x::Send, !{F4}
vk1D & f::Send, {Blind}{F13}
vk1D & c::Send, {Blind}+{F14}
vk1D & Ins::Send, {Blind}{F15}
vk1D & r::Send, {Blind}^{r}
vk1D & d::Click, D
vk1D & d Up::Click, U
vk1D & s::Click, R, D
vk1D & s Up::Click, R, U
vk1D & w::Click, WU, 8
vk1D & e::Click, WD, 8
vk1D & q::
  WinGetPos, X, Y, W, H, A
  MouseGetPos, offsetX, offsetY
  MX := min( W, 1920 - X ) - 1
  MY := min( H, 1080 - Y ) - 1
  abov_B := offsetY/MY + offsetX/MX > 1
  abov_W := offsetY/MY - offsetX/MX > 0
  MouseMove, ( abov_B/2 + !abov_B * abov_W ) * MX, ( !abov_B/2 + abov_B * !abov_W ) * MY
  return
vk1D & z::
  WinGetPos, X, Y, W, H, A
  MouseMove, min( W, 1920 - X ) - 30, min( H, 1080 - Y ) / 2
  Click, Middle
  MouseMove, 0, 30, 0, R
  return
vk1D & h::
vk1D & j::
vk1D & k::
vk1D & l::
  dX := 0, dY := 0, dS := 16, Delay := 80
  while ( GetKeyState( "h", "P" ) || GetKeyState( "j", "P" ) || GetKeyState( "k", "P" ) || GetKeyState( "l", "P" ) ) {
    CoordMode, Mouse, Screen
    dS := GetKeyState( "Ctrl", "P" ) ? 1 : dS
    dY := ( GetKeyState( "j", "P" ) - GetKeyState( "k", "P" ) ) * dS
    dX := ( GetKeyState( "l", "P" ) - GetKeyState( "h", "P" ) ) * dS
    dS := ( dX != 0 || dY != 0 ) ? ( ( dS < 80 ) ? dS * 1.14 : dS ) : 16
    Delay := ( dX != 0 || dY != 0 ) ? ( Delay > 20 ? Delay * 0.94 : Delay ): 80
    MouseMove, dX, dY, 0, R
    Sleep, Delay
  }
  return
