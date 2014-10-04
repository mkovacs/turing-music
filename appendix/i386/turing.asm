; emulator for 5-state 2-symbol Turing machines
; Copyright 2014 Mate J Kovacs

bits 16
org 0x0100

  ; set MCGA mode
  mov   al, 0x13
  int   0x10
  ; address video memory
  push  0xA000
  pop   es
  ; state
  xor   cl, cl
  ; head position
  mov   si, 160 + 320 * 100
turing_loop:
  ; compute look-up table index based on current state and bit on tape
  mov   ah, [es:si]
  sahf
  rcl   cl, 1
  ; write to tape
  mov   ax, [cs:flip_table]
  shr   ax, cl
  and   al, 0x01
  xor   [es:si], al
  ; step tape
  mov   ax, [cs:step_table]
  shr   ax, cl
  and   ax, 0x01
  jnz   step_left
  mov   ax, -1
step_left:
  add   si, ax
  ; update state
  mov   al, cl
  add   cl, al
  add   cl, al
  mov   eax, [cs:state_table]
  shr   eax, cl
  and   al, 0x07
  mov   cl, al
  ; slowdown
  mov   bx, 1000
busy_loop:
  dec   bx
  jnz   busy_loop
  ; exit on ESC
  in    al, 60h
  dec   al
  jz    exit
  ; that's it
  jmp   turing_loop
exit:
  ret

section .data

; encoding of the Turing machine
state_table:
  dd    0x2111041A
flip_table:
  dw    0x03DD
step_table:
  dw    0x00D2
